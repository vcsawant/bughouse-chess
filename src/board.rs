use crate::bitboard::{BitBoard, EMPTY};
use crate::board_builder::BoardBuilder;
use crate::castle_rights::CastleRights;
use crate::chess_move::ChessMove;
use crate::color::{Color, ALL_COLORS, NUM_COLORS};
use crate::error::Error;
use crate::file::File;
use crate::magic::{
    between, get_adjacent_files, get_bishop_moves, get_bishop_rays, get_castle_moves, get_file,
    get_king_moves, get_knight_moves, get_pawn_attacks, get_pawn_dest_double_moves,
    get_pawn_source_double_moves, get_rank, get_rook_moves, get_rook_rays,
};
use crate::movegen::*;
use crate::piece::{Piece, ALL_PIECES, NUM_PIECES};
use crate::rank::Rank;
use crate::reserve::Reserve;
use crate::square::{Square, ALL_SQUARES};
use crate::zobrist::Zobrist;
use std::convert::{TryFrom, TryInto};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::str::FromStr;

/// A representation of a bughouse chess board.
///
/// Extends the standard chess board with:
/// - `reserves`: pieces available for dropping (one per color)
/// - `promoted`: bitboard tracking which squares contain promoted pieces
///   (promoted pieces demote to pawns when captured in bughouse)
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub struct Board {
    pieces: [BitBoard; NUM_PIECES],
    color_combined: [BitBoard; NUM_COLORS],
    combined: BitBoard,
    side_to_move: Color,
    castle_rights: [CastleRights; NUM_COLORS],
    pinned: BitBoard,
    checkers: BitBoard,
    hash: u64,
    en_passant: Option<Square>,
    reserves: [Reserve; NUM_COLORS],
    promoted: BitBoard,
}

/// What is the status of this game?
///
/// In bughouse, there is no checkmate or stalemate. Instead, the game ends
/// when a king is capturable (the side to move can take the opponent's king).
#[derive(Copy, Clone, PartialEq, PartialOrd, Debug)]
pub enum BoardStatus {
    Ongoing,
    /// The side to move can capture the opponent's king — game over.
    KingCapturable,
}

/// Construct the initial position.
impl Default for Board {
    #[inline]
    fn default() -> Board {
        Board::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")
            .expect("Valid Position")
    }
}

impl Hash for Board {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.hash.hash(state);
    }
}

impl Board {
    /// Construct a new `Board` that is completely empty.
    /// Note: This does NOT give you the initial position.  Just a blank slate.
    fn new() -> Board {
        Board {
            pieces: [EMPTY; NUM_PIECES],
            color_combined: [EMPTY; NUM_COLORS],
            combined: EMPTY,
            side_to_move: Color::White,
            castle_rights: [CastleRights::NoRights; NUM_COLORS],
            pinned: EMPTY,
            checkers: EMPTY,
            hash: 0,
            en_passant: None,
            reserves: [Reserve::new(); NUM_COLORS],
            promoted: EMPTY,
        }
    }

    /// Construct a board from a FEN string.
    ///
    /// ```
    /// use bughouse_chess::Board;
    /// use std::str::FromStr;
    /// # use bughouse_chess::Error;
    ///
    /// # fn main() -> Result<(), Error> {
    ///
    /// // This is no longer supported
    /// let init_position = Board::from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_owned()).expect("Valid FEN");
    /// assert_eq!(init_position, Board::default());
    ///
    /// // This is the new way
    /// let init_position_2 = Board::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")?;
    /// assert_eq!(init_position_2, Board::default());
    /// # Ok(())
    /// # }
    /// ```
    #[deprecated(since = "3.1.0", note = "please use `Board::from_str(fen)?` instead")]
    #[inline]
    pub fn from_fen(fen: String) -> Option<Board> {
        Board::from_str(&fen).ok()
    }

    #[deprecated(
        since = "3.0.0",
        note = "please use the MoveGen structure instead.  It is faster and more idiomatic."
    )]
    #[inline]
    pub fn enumerate_moves(&self, moves: &mut [ChessMove; 256]) -> usize {
        let movegen = MoveGen::new_legal(self);
        let mut size = 0;
        for m in movegen {
            moves[size] = m;
            size += 1;
        }
        size
    }

    /// Is the opponent's king capturable?
    ///
    /// In bughouse, the game ends when a king can be captured by the side to move.
    /// There is no checkmate or stalemate concept.
    ///
    /// ```
    /// use bughouse_chess::{Board, BoardStatus};
    ///
    /// let board = Board::default();
    /// assert_eq!(board.status(), BoardStatus::Ongoing);
    /// ```
    #[inline]
    pub fn status(&self) -> BoardStatus {
        let their_king_sq = self.king_square(!self.side_to_move);
        if self.attackers_to(their_king_sq) & self.color_combined(self.side_to_move) != EMPTY {
            BoardStatus::KingCapturable
        } else {
            BoardStatus::Ongoing
        }
    }

    /// Returns a bitboard of all pieces that attack a given square.
    #[inline]
    pub fn attackers_to(&self, square: Square) -> BitBoard {
        let mut attackers = EMPTY;
        let combined = *self.combined();

        // Knights
        attackers |= get_knight_moves(square) & self.pieces(Piece::Knight);

        // Pawns (attacked by white pawns from below, black pawns from above)
        attackers |= get_pawn_attacks(square, Color::White, self.pieces(Piece::Pawn) & self.color_combined(Color::Black));
        attackers |= get_pawn_attacks(square, Color::Black, self.pieces(Piece::Pawn) & self.color_combined(Color::White));

        // King
        attackers |= get_king_moves(square) & self.pieces(Piece::King);

        // Bishops and queens (diagonal)
        let bishop_attacks = get_bishop_moves(square, combined);
        attackers |= bishop_attacks & (self.pieces(Piece::Bishop) | self.pieces(Piece::Queen));

        // Rooks and queens (straight)
        let rook_attacks = get_rook_moves(square, combined);
        attackers |= rook_attacks & (self.pieces(Piece::Rook) | self.pieces(Piece::Queen));

        attackers
    }

    /// Grab the "combined" `BitBoard`.  This is a `BitBoard` with every piece.
    ///
    /// ```
    /// use bughouse_chess::{Board, BitBoard, Rank, get_rank};
    ///
    /// let board = Board::default();
    ///
    /// let combined_should_be = get_rank(Rank::First) |
    ///                          get_rank(Rank::Second) |
    ///                          get_rank(Rank::Seventh) |
    ///                          get_rank(Rank::Eighth);
    ///
    /// assert_eq!(*board.combined(), combined_should_be);
    /// ```
    #[inline]
    pub fn combined(&self) -> &BitBoard {
        &self.combined
    }

    /// Grab the "color combined" `BitBoard`.  This is a `BitBoard` with every piece of a particular
    /// color.
    ///
    /// ```
    /// use bughouse_chess::{Board, BitBoard, Rank, get_rank, Color};
    ///
    /// let board = Board::default();
    ///
    /// let white_pieces = get_rank(Rank::First) |
    ///                    get_rank(Rank::Second);
    ///
    /// let black_pieces = get_rank(Rank::Seventh) |
    ///                    get_rank(Rank::Eighth);
    ///
    /// assert_eq!(*board.color_combined(Color::White), white_pieces);
    /// assert_eq!(*board.color_combined(Color::Black), black_pieces);
    /// ```
    #[inline]
    pub fn color_combined(&self, color: Color) -> &BitBoard {
        unsafe { self.color_combined.get_unchecked(color.to_index()) }
    }

    /// Give me the `Square` the `color` king is on.
    ///
    /// ```
    /// use bughouse_chess::{Board, Square, Color};
    ///
    /// let board = Board::default();
    ///
    /// assert_eq!(board.king_square(Color::White), Square::E1);
    /// assert_eq!(board.king_square(Color::Black), Square::E8);
    /// ```
    #[inline]
    pub fn king_square(&self, color: Color) -> Square {
        (self.pieces(Piece::King) & self.color_combined(color)).to_square()
    }

    /// Grab the "pieces" `BitBoard`.  This is a `BitBoard` with every piece of a particular type.
    ///
    /// ```
    /// use bughouse_chess::{Board, BitBoard, Piece, Square};
    ///
    /// // The rooks should be in each corner of the board
    /// let rooks = BitBoard::from_square(Square::A1) |
    ///             BitBoard::from_square(Square::H1) |
    ///             BitBoard::from_square(Square::A8) |
    ///             BitBoard::from_square(Square::H8);
    ///
    /// let board = Board::default();
    ///
    /// assert_eq!(*board.pieces(Piece::Rook), rooks);
    /// ```
    #[inline]
    pub fn pieces(&self, piece: Piece) -> &BitBoard {
        unsafe { self.pieces.get_unchecked(piece.to_index()) }
    }

    /// Grab the `CastleRights` for a particular side.
    ///
    /// ```
    /// use bughouse_chess::{Board, Square, CastleRights, Color, ChessMove};
    ///
    /// let move1 = ChessMove::new(Square::A2,
    ///                            Square::A4,
    ///                            None);
    ///
    /// let move2 = ChessMove::new(Square::E7,
    ///                            Square::E5,
    ///                            None);
    ///
    /// let move3 = ChessMove::new(Square::A1,
    ///                            Square::A2,
    ///                            None);
    ///
    /// let move4 = ChessMove::new(Square::E8,
    ///                            Square::E7,
    ///                            None);
    ///
    /// let mut board = Board::default();
    ///
    /// assert_eq!(board.castle_rights(Color::White), CastleRights::Both);
    /// assert_eq!(board.castle_rights(Color::Black), CastleRights::Both);
    ///
    /// board = board.make_move_new(move1)
    ///              .make_move_new(move2)
    ///              .make_move_new(move3)
    ///              .make_move_new(move4);
    ///
    /// assert_eq!(board.castle_rights(Color::White), CastleRights::KingSide);
    /// assert_eq!(board.castle_rights(Color::Black), CastleRights::NoRights);
    /// ```
    #[inline]
    pub fn castle_rights(&self, color: Color) -> CastleRights {
        unsafe { *self.castle_rights.get_unchecked(color.to_index()) }
    }

    /// Add castle rights for a particular side.  Note: this can create an invalid position.
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn add_castle_rights(&mut self, color: Color, add: CastleRights) {
        unsafe {
            *self.castle_rights.get_unchecked_mut(color.to_index()) =
                self.castle_rights(color).add(add);
        }
    }

    /// Remove castle rights for a particular side.
    ///
    /// ```
    /// use bughouse_chess::{Board, CastleRights, Color};
    ///
    /// let mut board = Board::default();
    /// assert_eq!(board.castle_rights(Color::White), CastleRights::Both);
    ///
    /// board.remove_castle_rights(Color::White, CastleRights::KingSide);
    /// assert_eq!(board.castle_rights(Color::White), CastleRights::QueenSide);
    /// ```
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn remove_castle_rights(&mut self, color: Color, remove: CastleRights) {
        unsafe {
            *self.castle_rights.get_unchecked_mut(color.to_index()) =
                self.castle_rights(color).remove(remove);
        }
    }

    /// Who's turn is it?
    ///
    /// ```
    /// use bughouse_chess::{Board, Color};
    ///
    /// let mut board = Board::default();
    /// assert_eq!(board.side_to_move(), Color::White);
    /// ```
    #[inline]
    pub fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    /// Grab my `CastleRights`.
    ///
    /// ```
    /// use bughouse_chess::{Board, Color, CastleRights};
    ///
    /// let mut board = Board::default();
    /// board.remove_castle_rights(Color::White, CastleRights::KingSide);
    /// board.remove_castle_rights(Color::Black, CastleRights::QueenSide);
    ///
    /// assert_eq!(board.my_castle_rights(), board.castle_rights(Color::White));
    /// ```
    #[inline]
    pub fn my_castle_rights(&self) -> CastleRights {
        self.castle_rights(self.side_to_move())
    }

    /// Add to my `CastleRights`.  Note: This can make the position invalid.
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn add_my_castle_rights(&mut self, add: CastleRights) {
        let color = self.side_to_move();
        #[allow(deprecated)]
        self.add_castle_rights(color, add);
    }

    /// Remove some of my `CastleRights`.
    ///
    /// ```
    /// use bughouse_chess::{Board, CastleRights};
    ///
    /// let mut board = Board::default();
    /// assert_eq!(board.my_castle_rights(), CastleRights::Both);
    ///
    /// board.remove_my_castle_rights(CastleRights::KingSide);
    /// assert_eq!(board.my_castle_rights(), CastleRights::QueenSide);
    /// ```
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn remove_my_castle_rights(&mut self, remove: CastleRights) {
        let color = self.side_to_move();
        #[allow(deprecated)]
        self.remove_castle_rights(color, remove);
    }

    /// My opponents `CastleRights`.
    ///
    /// ```
    /// use bughouse_chess::{Board, Color, CastleRights};
    ///
    /// let mut board = Board::default();
    /// board.remove_castle_rights(Color::White, CastleRights::KingSide);
    /// board.remove_castle_rights(Color::Black, CastleRights::QueenSide);
    ///
    /// assert_eq!(board.their_castle_rights(), board.castle_rights(Color::Black));
    /// ```
    #[inline]
    pub fn their_castle_rights(&self) -> CastleRights {
        self.castle_rights(!self.side_to_move())
    }

    /// Add to my opponents `CastleRights`. Note: This can make the position invalid.
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn add_their_castle_rights(&mut self, add: CastleRights) {
        let color = !self.side_to_move();
        #[allow(deprecated)]
        self.add_castle_rights(color, add)
    }

    /// Remove some of my opponents `CastleRights`.
    ///
    /// ```
    /// use bughouse_chess::{Board, CastleRights};
    ///
    /// let mut board = Board::default();
    /// assert_eq!(board.their_castle_rights(), CastleRights::Both);
    ///
    /// board.remove_their_castle_rights(CastleRights::KingSide);
    /// assert_eq!(board.their_castle_rights(), CastleRights::QueenSide);
    /// ```
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn remove_their_castle_rights(&mut self, remove: CastleRights) {
        let color = !self.side_to_move();
        #[allow(deprecated)]
        self.remove_castle_rights(color, remove);
    }

    /// Add or remove a piece from the bitboards in this struct.
    fn xor(&mut self, piece: Piece, bb: BitBoard, color: Color) {
        unsafe {
            *self.pieces.get_unchecked_mut(piece.to_index()) ^= bb;
            *self.color_combined.get_unchecked_mut(color.to_index()) ^= bb;
            self.combined ^= bb;
            self.hash ^= Zobrist::piece(piece, bb.to_square(), color);
        }
    }

    /// For a chess UI: set a piece on a particular square.
    ///
    /// ```
    /// use bughouse_chess::{Board, Piece, Color, Square};
    ///
    /// let board = Board::default();
    ///
    /// let new_board = board.set_piece(Piece::Queen,
    ///                                 Color::White,
    ///                                 Square::E4)
    ///                      .expect("Valid Position");
    ///
    /// assert_eq!(new_board.pieces(Piece::Queen).count(), 3);
    /// ```
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn set_piece(&self, piece: Piece, color: Color, square: Square) -> Option<Board> {
        let mut result = *self;
        let square_bb = BitBoard::from_square(square);
        match self.piece_on(square) {
            None => result.xor(piece, square_bb, color),
            Some(x) => {
                // remove x from the bitboard
                if self.color_combined(Color::White) & square_bb == square_bb {
                    result.xor(x, square_bb, Color::White);
                } else {
                    result.xor(x, square_bb, Color::Black);
                }
                // add piece to the bitboard
                result.xor(piece, square_bb, color);
            }
        }

        // In bughouse, opponent can be in check, so skip that validation.
        result.update_pin_info();
        Some(result)
    }

    /// For a chess UI: clear a particular square.
    ///
    /// ```
    /// use bughouse_chess::{Board, Square, Piece};
    ///
    /// let board = Board::default();
    ///
    /// let new_board = board.clear_square(Square::A1)
    ///                      .expect("Valid Position");
    ///
    /// assert_eq!(new_board.pieces(Piece::Rook).count(), 3);
    /// ```
    #[deprecated(
        since = "3.1.0",
        note = "When doing board setup, use the BoardBuilder structure.  It ensures you don't end up with an invalid position."
    )]
    #[inline]
    pub fn clear_square(&self, square: Square) -> Option<Board> {
        let mut result = *self;
        let square_bb = BitBoard::from_square(square);
        match self.piece_on(square) {
            None => {}
            Some(x) => {
                // remove x from the bitboard
                if self.color_combined(Color::White) & square_bb == square_bb {
                    result.xor(x, square_bb, Color::White);
                } else {
                    result.xor(x, square_bb, Color::Black);
                }
            }
        }

        // In bughouse, opponent can be in check, so skip that validation.
        result.update_pin_info();
        Some(result)
    }

    /// Switch the color of the player without actually making a move.  Returns None if the current
    /// player is in check.
    ///
    /// Note that this erases the en-passant information, so applying this function twice does not
    /// always give the same result back.
    ///
    /// ```
    /// use bughouse_chess::{Board, Color};
    ///
    /// let board = Board::default();
    ///
    /// assert_eq!(board.side_to_move(), Color::White);
    ///
    /// let new_board = board.null_move().expect("Valid Position");
    ///
    /// assert_eq!(new_board.side_to_move(), Color::Black);
    /// ```
    #[inline]
    pub fn null_move(&self) -> Option<Board> {
        // In bughouse, null moves are always allowed (no check restriction).
        let mut result = *self;
        result.side_to_move = !result.side_to_move;
        result.remove_ep();
        result.update_pin_info();
        Some(result)
    }

    /// Does this board "make sense"?
    /// Do all the pieces make sense, do the bitboards combine correctly, etc?
    /// This is for sanity checking.
    ///
    /// ```
    /// use bughouse_chess::{Board, Color, Piece, Square};
    ///
    /// let board = Board::default();
    ///
    /// assert_eq!(board.is_sane(), true);
    ///
    /// // Remove the king
    /// let bad_board = board.clear_square(Square::E1).expect("Valid Position");
    /// assert_eq!(bad_board.is_sane(), false);
    /// ```
    pub fn is_sane(&self) -> bool {
        // make sure there is no square with multiple pieces on it
        for x in ALL_PIECES.iter() {
            for y in ALL_PIECES.iter() {
                if *x != *y {
                    if self.pieces(*x) & self.pieces(*y) != EMPTY {
                        return false;
                    }
                }
            }
        }

        // make sure the colors don't overlap, either
        if self.color_combined(Color::White) & self.color_combined(Color::Black) != EMPTY {
            return false;
        }

        // grab all the pieces by OR'ing together each piece() BitBoard
        let combined = ALL_PIECES
            .iter()
            .fold(EMPTY, |cur, next| cur | self.pieces(*next));

        // make sure that's equal to the combined bitboard
        if combined != *self.combined() {
            return false;
        }

        // make sure there is exactly one white king
        if (self.pieces(Piece::King) & self.color_combined(Color::White)).popcnt() != 1 {
            return false;
        }

        // make sure there is exactly one black king
        if (self.pieces(Piece::King) & self.color_combined(Color::Black)).popcnt() != 1 {
            return false;
        }

        // make sure the en_passant square has a pawn on it of the right color
        match self.en_passant {
            None => {}
            Some(x) => {
                if self.pieces(Piece::Pawn)
                    & self.color_combined(!self.side_to_move)
                    & BitBoard::from_square(x)
                    == EMPTY
                {
                    return false;
                }
            }
        }

        // In bughouse, the opponent CAN be in check when it's your turn
        // (you chose not to escape), so we skip that validation.

        // for each color, verify that, if they have castle rights, that they haven't moved their
        // rooks or king
        for color in ALL_COLORS.iter() {
            // get the castle rights
            let castle_rights = self.castle_rights(*color);

            // the castle rights object will tell us which rooks shouldn't have moved yet.
            // verify there are rooks on all those squares
            if castle_rights.unmoved_rooks(*color)
                & self.pieces(Piece::Rook)
                & self.color_combined(*color)
                != castle_rights.unmoved_rooks(*color)
            {
                return false;
            }
            // if we have castle rights, make sure we have a king on the (E, {1,8}) square,
            // depending on the color
            if castle_rights != CastleRights::NoRights {
                if self.pieces(Piece::King) & self.color_combined(*color)
                    != get_file(File::E) & get_rank(color.to_my_backrank())
                {
                    return false;
                }
            }
        }

        // In bughouse, kings CAN be adjacent (king can move next to enemy king),
        // so we skip the "kings aren't touching" check.

        // it checks out
        return true;
    }

    /// Get a hash of the board, including reserves (for bughouse position identity).
    #[inline]
    pub fn get_hash(&self) -> u64 {
        self.hash
            ^ if let Some(ep) = self.en_passant {
                Zobrist::en_passant(ep.get_file(), !self.side_to_move)
            } else {
                0
            }
            ^ Zobrist::castles(
                self.castle_rights[self.side_to_move.to_index()],
                self.side_to_move,
            )
            ^ Zobrist::castles(
                self.castle_rights[(!self.side_to_move).to_index()],
                !self.side_to_move,
            )
            ^ if self.side_to_move == Color::Black {
                Zobrist::color()
            } else {
                0
            }
            ^ self.reserve_hash()
    }

    /// Compute a hash contribution from reserve state.
    /// Uses simple mixing so same board + different reserves = different hash.
    fn reserve_hash(&self) -> u64 {
        // Use a set of prime multipliers for deterministic hashing of reserve counts.
        // This doesn't need to be in a Zobrist table since reserves change rarely.
        const PRIMES: [[u64; 5]; 2] = [
            // White: Pawn, Knight, Bishop, Rook, Queen
            [0x9E3779B97F4A7C15, 0x6C62272E07BB0142, 0x94D049BB133111EB,
             0xBF58476D1CE4E5B9, 0xD6E8FEB86659FD93],
            // Black: Pawn, Knight, Bishop, Rook, Queen
            [0x4BE98134A5E5D9A1, 0xC42E9F2D28F50A31, 0x7C3D8F5B12A3E6C9,
             0xE97E4BF8B9A2D3F5, 0x2A6B5D9F3C1E8A47],
        ];
        let mut h: u64 = 0;
        for color_idx in 0..2 {
            let reserve = &self.reserves[color_idx];
            let pieces = [Piece::Pawn, Piece::Knight, Piece::Bishop, Piece::Rook, Piece::Queen];
            for (i, piece) in pieces.iter().enumerate() {
                let count = reserve.count(*piece) as u64;
                h ^= count.wrapping_mul(PRIMES[color_idx][i]);
            }
        }
        h
    }

    /// Get a pawn hash of the board (a hash that only changes on color change and pawn moves).
    ///
    /// Currently not implemented...
    #[inline]
    pub fn get_pawn_hash(&self) -> u64 {
        0
    }

    /// What piece is on a particular `Square`?  Is there even one?
    ///
    /// ```
    /// use bughouse_chess::{Board, Piece, Square};
    ///
    /// let board = Board::default();
    ///
    /// assert_eq!(board.piece_on(Square::A1), Some(Piece::Rook));
    /// assert_eq!(board.piece_on(Square::D4), None);
    /// ```
    #[inline]
    pub fn piece_on(&self, square: Square) -> Option<Piece> {
        let opp = BitBoard::from_square(square);
        if self.combined() & opp == EMPTY {
            None
        } else {
            //naiive algorithm
            /*
            for p in ALL_PIECES {
                if self.pieces(*p) & opp {
                    return p;
                }
            } */
            if (self.pieces(Piece::Pawn) ^ self.pieces(Piece::Knight) ^ self.pieces(Piece::Bishop))
                & opp
                != EMPTY
            {
                if self.pieces(Piece::Pawn) & opp != EMPTY {
                    Some(Piece::Pawn)
                } else if self.pieces(Piece::Knight) & opp != EMPTY {
                    Some(Piece::Knight)
                } else {
                    Some(Piece::Bishop)
                }
            } else {
                if self.pieces(Piece::Rook) & opp != EMPTY {
                    Some(Piece::Rook)
                } else if self.pieces(Piece::Queen) & opp != EMPTY {
                    Some(Piece::Queen)
                } else {
                    Some(Piece::King)
                }
            }
        }
    }

    /// What color piece is on a particular square?
    #[inline]
    pub fn color_on(&self, square: Square) -> Option<Color> {
        if (self.color_combined(Color::White) & BitBoard::from_square(square)) != EMPTY {
            Some(Color::White)
        } else if (self.color_combined(Color::Black) & BitBoard::from_square(square)) != EMPTY {
            Some(Color::Black)
        } else {
            None
        }
    }

    /// Unset the en_passant square.
    fn remove_ep(&mut self) {
        self.en_passant = None;
    }

    /// Give me the en_passant square, if it exists.
    ///
    /// ```
    /// use bughouse_chess::{Board, ChessMove, Square};
    ///
    /// let move1 = ChessMove::new(Square::D2,
    ///                            Square::D4,
    ///                            None);
    ///
    /// let move2 = ChessMove::new(Square::H7,
    ///                            Square::H5,
    ///                            None);
    ///
    /// let move3 = ChessMove::new(Square::D4,
    ///                            Square::D5,
    ///                            None);
    ///
    /// let move4 = ChessMove::new(Square::E7,
    ///                            Square::E5,
    ///                            None);
    ///
    /// let board = Board::default().make_move_new(move1)
    ///                             .make_move_new(move2)
    ///                             .make_move_new(move3)
    ///                             .make_move_new(move4);
    ///
    /// assert_eq!(board.en_passant(), Some(Square::E5));
    /// ```
    #[inline]
    pub fn en_passant(self) -> Option<Square> {
        self.en_passant
    }

    /// Get the reserve for a particular color.
    #[inline]
    pub fn reserves(&self, color: Color) -> &Reserve {
        unsafe { &self.reserves.get_unchecked(color.to_index()) }
    }

    /// Get the promoted bitboard (squares containing promoted pieces).
    #[inline]
    pub fn promoted(&self) -> &BitBoard {
        &self.promoted
    }

    /// Is the piece on this square a promoted piece?
    #[inline]
    pub fn is_promoted(&self, square: Square) -> bool {
        self.promoted & BitBoard::from_square(square) != EMPTY
    }

    /// Add a piece to a color's reserve.
    #[inline]
    pub fn add_to_reserve(&mut self, color: Color, piece: Piece) {
        unsafe {
            self.reserves.get_unchecked_mut(color.to_index()).add(piece);
        }
    }

    /// Set the en_passant square.  Note: This must only be called when self.en_passant is already
    /// None.
    fn set_ep(&mut self, sq: Square) {
        // Only set self.en_passant if the pawn can actually be captured next move.
        if get_adjacent_files(sq.get_file())
            & get_rank(sq.get_rank())
            & self.pieces(Piece::Pawn)
            & self.color_combined(!self.side_to_move)
            != EMPTY
        {
            self.en_passant = Some(sq);
        }
    }

    /// Is a particular move legal?  This function is very slow, but will work on unsanitized
    /// input.
    ///
    /// ```
    /// use bughouse_chess::{Board, ChessMove, Square, MoveGen};
    ///
    /// let move1 = ChessMove::new(Square::E2,
    ///                            Square::E4,
    ///                            None);
    ///
    /// let move2 = ChessMove::new(Square::E2,
    ///                            Square::E5,
    ///                            None);
    ///
    /// let board = Board::default();
    ///
    /// assert_eq!(board.legal(move1), true);
    /// assert_eq!(board.legal(move2), false);
    /// ```
    #[inline]
    pub fn legal(&self, m: ChessMove) -> bool {
        MoveGen::new_legal(&self).find(|x| *x == m).is_some()
    }

    /// Make a chess move onto a new board.
    ///
    /// panic!() if king is captured.
    ///
    /// ```
    /// use bughouse_chess::{Board, ChessMove, Square, Color};
    ///
    /// let m = ChessMove::new(Square::D2,
    ///                        Square::D4,
    ///                        None);
    ///
    /// let board = Board::default();
    /// assert_eq!(board.make_move_new(m).side_to_move(), Color::Black);
    /// ```
    #[inline]
    pub fn make_move_new(&self, m: ChessMove) -> Board {
        let mut result = *self;
        result.remove_ep();
        result.checkers = EMPTY;
        result.pinned = EMPTY;
        let source = m.get_source();
        let dest = m.get_dest();

        let source_bb = BitBoard::from_square(source);
        let dest_bb = BitBoard::from_square(dest);
        let move_bb = source_bb ^ dest_bb;
        let moved = self.piece_on(source).unwrap();

        result.xor(moved, source_bb, self.side_to_move);
        result.xor(moved, dest_bb, self.side_to_move);
        if let Some(captured) = self.piece_on(dest) {
            result.xor(captured, dest_bb, !self.side_to_move);
        }

        #[allow(deprecated)]
        result.remove_their_castle_rights(CastleRights::square_to_castle_rights(
            !self.side_to_move,
            dest,
        ));

        #[allow(deprecated)]
        result.remove_my_castle_rights(CastleRights::square_to_castle_rights(
            self.side_to_move,
            source,
        ));

        let opp_king = result.pieces(Piece::King) & result.color_combined(!result.side_to_move);

        let castles = moved == Piece::King && (move_bb & get_castle_moves()) == move_bb;

        let ksq = opp_king.to_square();

        const CASTLE_ROOK_START: [File; 8] = [
            File::A,
            File::A,
            File::A,
            File::A,
            File::H,
            File::H,
            File::H,
            File::H,
        ];
        const CASTLE_ROOK_END: [File; 8] = [
            File::D,
            File::D,
            File::D,
            File::D,
            File::F,
            File::F,
            File::F,
            File::F,
        ];

        if moved == Piece::Knight {
            result.checkers ^= get_knight_moves(ksq) & dest_bb;
        } else if moved == Piece::Pawn {
            if let Some(Piece::Knight) = m.get_promotion() {
                result.xor(Piece::Pawn, dest_bb, self.side_to_move);
                result.xor(Piece::Knight, dest_bb, self.side_to_move);
                result.checkers ^= get_knight_moves(ksq) & dest_bb;
            } else if let Some(promotion) = m.get_promotion() {
                result.xor(Piece::Pawn, dest_bb, self.side_to_move);
                result.xor(promotion, dest_bb, self.side_to_move);
            } else if (source_bb & get_pawn_source_double_moves()) != EMPTY
                && (dest_bb & get_pawn_dest_double_moves()) != EMPTY
            {
                result.set_ep(dest);
                result.checkers ^= get_pawn_attacks(ksq, !result.side_to_move, dest_bb);
            } else if Some(dest.ubackward(self.side_to_move)) == self.en_passant {
                result.xor(
                    Piece::Pawn,
                    BitBoard::from_square(dest.ubackward(self.side_to_move)),
                    !self.side_to_move,
                );
                result.checkers ^= get_pawn_attacks(ksq, !result.side_to_move, dest_bb);
            } else {
                result.checkers ^= get_pawn_attacks(ksq, !result.side_to_move, dest_bb);
            }
        } else if castles {
            let my_backrank = self.side_to_move.to_my_backrank();
            let index = dest.get_file().to_index();
            let start = BitBoard::set(my_backrank, unsafe {
                *CASTLE_ROOK_START.get_unchecked(index)
            });
            let end = BitBoard::set(my_backrank, unsafe {
                *CASTLE_ROOK_END.get_unchecked(index)
            });
            result.xor(Piece::Rook, start, self.side_to_move);
            result.xor(Piece::Rook, end, self.side_to_move);
        }
        // now, lets see if we're in check or pinned
        let attackers = result.color_combined(result.side_to_move)
            & ((get_bishop_rays(ksq)
                & (result.pieces(Piece::Bishop) | result.pieces(Piece::Queen)))
                | (get_rook_rays(ksq)
                    & (result.pieces(Piece::Rook) | result.pieces(Piece::Queen))));

        for sq in attackers {
            let between = between(sq, ksq) & result.combined();
            if between == EMPTY {
                result.checkers ^= BitBoard::from_square(sq);
            } else if between.popcnt() == 1 {
                result.pinned ^= between;
            }
        }

        result.side_to_move = !result.side_to_move;
        result
    }

    /// Make a chess move onto an already allocated `Board`.
    ///
    /// panic!() if king is captured.
    ///
    /// ```
    /// use bughouse_chess::{Board, ChessMove, Square, Color};
    ///
    /// let m = ChessMove::new(Square::D2,
    ///                        Square::D4,
    ///                        None);
    ///
    /// let board = Board::default();
    /// let mut result = Board::default();
    /// board.make_move(m, &mut result);
    /// assert_eq!(result.side_to_move(), Color::Black);
    /// ```
    #[inline]
    pub fn make_move(&self, m: ChessMove, result: &mut Board) {
        *result = *self;
        result.remove_ep();
        result.checkers = EMPTY;
        result.pinned = EMPTY;
        let source = m.get_source();
        let dest = m.get_dest();

        let source_bb = BitBoard::from_square(source);
        let dest_bb = BitBoard::from_square(dest);
        let move_bb = source_bb ^ dest_bb;
        let moved = self.piece_on(source).unwrap();

        result.xor(moved, source_bb, self.side_to_move);
        result.xor(moved, dest_bb, self.side_to_move);
        if let Some(captured) = self.piece_on(dest) {
            result.xor(captured, dest_bb, !self.side_to_move);
        }

        #[allow(deprecated)]
        result.remove_their_castle_rights(CastleRights::square_to_castle_rights(
            !self.side_to_move,
            dest,
        ));

        #[allow(deprecated)]
        result.remove_my_castle_rights(CastleRights::square_to_castle_rights(
            self.side_to_move,
            source,
        ));

        let opp_king = result.pieces(Piece::King) & result.color_combined(!result.side_to_move);

        let castles = moved == Piece::King && (move_bb & get_castle_moves()) == move_bb;

        let ksq = opp_king.to_square();

        const CASTLE_ROOK_START: [File; 8] = [
            File::A,
            File::A,
            File::A,
            File::A,
            File::H,
            File::H,
            File::H,
            File::H,
        ];
        const CASTLE_ROOK_END: [File; 8] = [
            File::D,
            File::D,
            File::D,
            File::D,
            File::F,
            File::F,
            File::F,
            File::F,
        ];

        if moved == Piece::Knight {
            result.checkers ^= get_knight_moves(ksq) & dest_bb;
        } else if moved == Piece::Pawn {
            if let Some(Piece::Knight) = m.get_promotion() {
                result.xor(Piece::Pawn, dest_bb, self.side_to_move);
                result.xor(Piece::Knight, dest_bb, self.side_to_move);
                result.checkers ^= get_knight_moves(ksq) & dest_bb;
            } else if let Some(promotion) = m.get_promotion() {
                result.xor(Piece::Pawn, dest_bb, self.side_to_move);
                result.xor(promotion, dest_bb, self.side_to_move);
            } else if (source_bb & get_pawn_source_double_moves()) != EMPTY
                && (dest_bb & get_pawn_dest_double_moves()) != EMPTY
            {
                result.set_ep(dest);
                result.checkers ^= get_pawn_attacks(ksq, !result.side_to_move, dest_bb);
            } else if Some(dest.ubackward(self.side_to_move)) == self.en_passant {
                result.xor(
                    Piece::Pawn,
                    BitBoard::from_square(dest.ubackward(self.side_to_move)),
                    !self.side_to_move,
                );
                result.checkers ^= get_pawn_attacks(ksq, !result.side_to_move, dest_bb);
            } else {
                result.checkers ^= get_pawn_attacks(ksq, !result.side_to_move, dest_bb);
            }
        } else if castles {
            let my_backrank = self.side_to_move.to_my_backrank();
            let index = dest.get_file().to_index();
            let start = BitBoard::set(my_backrank, unsafe {
                *CASTLE_ROOK_START.get_unchecked(index)
            });
            let end = BitBoard::set(my_backrank, unsafe {
                *CASTLE_ROOK_END.get_unchecked(index)
            });
            result.xor(Piece::Rook, start, self.side_to_move);
            result.xor(Piece::Rook, end, self.side_to_move);
        }
        // now, lets see if we're in check or pinned
        let attackers = result.color_combined(result.side_to_move)
            & ((get_bishop_rays(ksq)
                & (result.pieces(Piece::Bishop) | result.pieces(Piece::Queen)))
                | (get_rook_rays(ksq)
                    & (result.pieces(Piece::Rook) | result.pieces(Piece::Queen))));

        for sq in attackers {
            let between = between(sq, ksq) & result.combined();
            if between == EMPTY {
                result.checkers ^= BitBoard::from_square(sq);
            } else if between.popcnt() == 1 {
                result.pinned ^= between;
            }
        }

        result.side_to_move = !result.side_to_move;
    }

    /// Make a move and return (new_board, captured_piece_info).
    ///
    /// `captured_piece_info` is `Some((piece, was_promoted))` if a piece was captured.
    /// In bughouse, promoted pieces demote to pawns when captured, so callers should
    /// check `was_promoted` and add a Pawn (not the captured piece type) to the partner's reserve.
    #[inline]
    pub fn make_move_with_capture(&self, m: ChessMove) -> (Board, Option<(Piece, bool)>) {
        let dest = m.get_dest();
        let source = m.get_source();

        // Determine what was captured before making the move
        let captured_info = if let Some(captured_piece) = self.piece_on(dest) {
            let was_promoted = self.is_promoted(dest);
            Some((captured_piece, was_promoted))
        } else if self.piece_on(source) == Some(Piece::Pawn)
            && source.get_file() != dest.get_file()
            && self.piece_on(dest).is_none()
        {
            // En passant capture
            Some((Piece::Pawn, false))
        } else {
            None
        };

        let mut new_board = self.make_move_new(m);

        // Track promoted pieces through moves
        let source_bb = BitBoard::from_square(source);
        let dest_bb = BitBoard::from_square(dest);

        // Clear promoted bit on captured square (piece was taken off)
        new_board.promoted &= !dest_bb;

        // If the moved piece was promoted, carry the bit to dest
        if self.promoted & source_bb != EMPTY {
            // Clear old position (already cleared by move)
            new_board.promoted &= !source_bb;
            // Set new position (unless it's a pawn that can't be promoted — shouldn't happen)
            new_board.promoted |= dest_bb;
        }

        // If this is a pawn promotion, mark the destination as promoted
        if self.piece_on(source) == Some(Piece::Pawn) && m.get_promotion().is_some() {
            new_board.promoted |= dest_bb;
        }

        (new_board, captured_info)
    }

    /// Place a piece from reserves onto an empty square.
    ///
    /// Returns `Some(new_board)` if the drop is legal, `None` otherwise.
    /// A drop is legal when:
    /// - The square is empty
    /// - The piece is in the side_to_move's reserves
    /// - Pawns cannot be dropped on rank 1 or rank 8
    /// - Kings cannot be dropped
    pub fn make_drop_new(&self, piece: Piece, square: Square) -> Option<Board> {
        // Cannot drop a king
        if piece == Piece::King {
            return None;
        }

        // Square must be empty
        if self.combined() & BitBoard::from_square(square) != EMPTY {
            return None;
        }

        // Pawns cannot be dropped on rank 1 or 8
        if piece == Piece::Pawn
            && (square.get_rank() == Rank::First || square.get_rank() == Rank::Eighth)
        {
            return None;
        }

        // Must have the piece in reserves
        let color = self.side_to_move;
        if self.reserves[color.to_index()].count(piece) == 0 {
            return None;
        }

        let mut result = *self;
        result.remove_ep();
        result.checkers = EMPTY;
        result.pinned = EMPTY;

        // Remove piece from reserves
        result.reserves[color.to_index()].remove(piece);

        // Place piece on the board
        let square_bb = BitBoard::from_square(square);
        result.xor(piece, square_bb, color);

        // Update check/pin info for the new position
        let ksq = (result.pieces(Piece::King) & result.color_combined(!result.side_to_move)).to_square();

        // Check if the dropped piece gives check
        if piece == Piece::Knight {
            result.checkers ^= get_knight_moves(ksq) & square_bb;
        } else if piece == Piece::Pawn {
            result.checkers ^= get_pawn_attacks(ksq, !result.side_to_move, square_bb);
        }

        // Check for sliding piece checks and pins
        let attackers = result.color_combined(result.side_to_move)
            & ((get_bishop_rays(ksq)
                & (result.pieces(Piece::Bishop) | result.pieces(Piece::Queen)))
                | (get_rook_rays(ksq)
                    & (result.pieces(Piece::Rook) | result.pieces(Piece::Queen))));

        for sq in attackers {
            let between = between(sq, ksq) & result.combined();
            if between == EMPTY {
                result.checkers ^= BitBoard::from_square(sq);
            } else if between.popcnt() == 1 {
                result.pinned ^= between;
            }
        }

        result.side_to_move = !result.side_to_move;
        Some(result)
    }

    /// Update the pin information.
    fn update_pin_info(&mut self) {
        self.pinned = EMPTY;
        self.checkers = EMPTY;

        let ksq = (self.pieces(Piece::King) & self.color_combined(self.side_to_move)).to_square();

        let pinners = self.color_combined(!self.side_to_move)
            & ((get_bishop_rays(ksq) & (self.pieces(Piece::Bishop) | self.pieces(Piece::Queen)))
                | (get_rook_rays(ksq) & (self.pieces(Piece::Rook) | self.pieces(Piece::Queen))));

        for sq in pinners {
            let between = between(sq, ksq) & self.combined();
            if between == EMPTY {
                self.checkers ^= BitBoard::from_square(sq);
            } else if between.popcnt() == 1 {
                self.pinned ^= between;
            }
        }

        self.checkers ^= get_knight_moves(ksq)
            & self.color_combined(!self.side_to_move)
            & self.pieces(Piece::Knight);

        self.checkers ^= get_pawn_attacks(
            ksq,
            self.side_to_move,
            self.color_combined(!self.side_to_move) & self.pieces(Piece::Pawn),
        );
    }

    /// Give me the `BitBoard` of my pinned pieces.
    #[inline]
    pub fn pinned(&self) -> &BitBoard {
        &self.pinned
    }

    /// Give me the `Bitboard` of the pieces putting me in check.
    #[inline]
    pub fn checkers(&self) -> &BitBoard {
        &self.checkers
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let fen: BoardBuilder = self.into();
        write!(f, "{}", fen)
    }
}

impl TryFrom<&BoardBuilder> for Board {
    type Error = Error;

    fn try_from(fen: &BoardBuilder) -> Result<Self, Self::Error> {
        let mut board = Board::new();

        for sq in ALL_SQUARES.iter() {
            if let Some((piece, color)) = fen[*sq] {
                board.xor(piece, BitBoard::from_square(*sq), color);
            }
        }

        board.side_to_move = fen.get_side_to_move();

        if let Some(ep) = fen.get_en_passant() {
            board.side_to_move = !board.side_to_move;
            board.set_ep(ep);
            board.side_to_move = !board.side_to_move;
        }

        #[allow(deprecated)]
        board.add_castle_rights(Color::White, fen.get_castle_rights(Color::White));
        #[allow(deprecated)]
        board.add_castle_rights(Color::Black, fen.get_castle_rights(Color::Black));

        // Copy reserves and promoted from the builder
        board.reserves = fen.get_reserves();
        board.promoted = fen.get_promoted();

        board.update_pin_info();

        if board.is_sane() {
            Ok(board)
        } else {
            Err(Error::InvalidBoard)
        }
    }
}

impl TryFrom<&mut BoardBuilder> for Board {
    type Error = Error;

    fn try_from(fen: &mut BoardBuilder) -> Result<Self, Self::Error> {
        (&*fen).try_into()
    }
}

impl TryFrom<BoardBuilder> for Board {
    type Error = Error;

    fn try_from(fen: BoardBuilder) -> Result<Self, Self::Error> {
        (&fen).try_into()
    }
}

impl FromStr for Board {
    type Err = Error;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        Ok(BoardBuilder::from_str(value)?.try_into()?)
    }
}

#[test]
fn test_null_move_en_passant() {
    let start =
        Board::from_str("rnbqkbnr/pppp2pp/8/4pP2/8/8/PPPP1PPP/RNBQKBNR w KQkq e6 0 0").unwrap();
    let expected =
        Board::from_str("rnbqkbnr/pppp2pp/8/4pP2/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 0").unwrap();
    assert_eq!(start.null_move().unwrap(), expected);
}
