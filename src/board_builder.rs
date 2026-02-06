use crate::bitboard::{BitBoard, EMPTY};
use crate::board::Board;
use crate::castle_rights::CastleRights;
use crate::color::{Color, NUM_COLORS};
use crate::error::Error;
use crate::file::{File, ALL_FILES};
use crate::piece::Piece;
use crate::rank::{Rank, ALL_RANKS};
use crate::reserve::Reserve;
use crate::square::{Square, ALL_SQUARES};

use std::fmt;
use std::ops::{Index, IndexMut};
use std::str::FromStr;

/// Represents a chess position that has *not* been validated for legality.
///
/// This structure is useful in the following cases:
/// * You are trying to build a chess board manually in code.
/// * The `Board` structure will try to keep the position fully legal, which will prevent you from
///   placing pieces arbitrarily.  This structure will not.
/// * You want to display the chess position in a UI.
/// * You want to convert between formats like FEN.
///
/// ```
/// use bughouse_chess::{BoardBuilder, Board, Square, Color, Piece};
/// use std::convert::TryFrom;
/// let mut position = BoardBuilder::new();
/// position.piece(Square::A1, Piece::King, Color::White);
/// position.piece(Square::A8, Piece::Rook, Color::Black);
/// position.piece(Square::D1, Piece::King, Color::Black);
///
/// // You can index the position by the square:
/// assert_eq!(position[Square::A1], Some((Piece::King, Color::White)));
///
/// // White is in check, but that's ok, it's white's turn to move.
/// assert!(Board::try_from(&position).is_ok());
///
/// // In bughouse, the opponent CAN be in check (you chose not to escape).
/// position.side_to_move(Color::Black);
/// assert!(Board::try_from(position).is_ok());
///
/// // One liners are possible with the builder pattern.
/// use std::convert::TryInto;
///
/// let res: Result<Board, _> = BoardBuilder::new()
///                        .piece(Square::A1, Piece::King, Color::White)
///                        .piece(Square::A8, Piece::King, Color::Black)
///                        .try_into();
/// assert!(res.is_ok());
/// ```
#[derive(Copy, Clone)]
pub struct BoardBuilder {
    pieces: [Option<(Piece, Color)>; 64],
    side_to_move: Color,
    castle_rights: [CastleRights; 2],
    en_passant: Option<File>,
    reserves: [Reserve; NUM_COLORS],
    promoted: BitBoard,
}

impl BoardBuilder {
    /// Construct a new, empty, BoardBuilder.
    ///
    /// * No pieces are on the board
    /// * `CastleRights` are empty for both sides
    /// * `en_passant` is not set
    /// * `side_to_move` is Color::White
    /// ```
    /// use bughouse_chess::{BoardBuilder, Board, Square, Color, Piece};
    /// use std::convert::TryInto;
    ///
    /// # use bughouse_chess::Error;
    /// # fn main() -> Result<(), Error> {
    /// let board: Board = BoardBuilder::new()
    ///     .piece(Square::A1, Piece::King, Color::White)
    ///     .piece(Square::A8, Piece::King, Color::Black)
    ///     .try_into()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn new() -> BoardBuilder {
        BoardBuilder {
            pieces: [None; 64],
            side_to_move: Color::White,
            castle_rights: [CastleRights::NoRights, CastleRights::NoRights],
            en_passant: None,
            reserves: [Reserve::new(); NUM_COLORS],
            promoted: EMPTY,
        }
    }

    /// Set up a board with everything pre-loaded.
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Board, Square, Color, Piece, CastleRights};
    /// use std::convert::TryInto;
    ///
    /// # use bughouse_chess::Error;
    /// # fn main() -> Result<(), Error> {
    /// let board: Board = BoardBuilder::setup(
    ///         &[
    ///             (Square::A1, Piece::King, Color::White),
    ///             (Square::H8, Piece::King, Color::Black)
    ///         ],
    ///         Color::Black,
    ///         CastleRights::NoRights,
    ///         CastleRights::NoRights,
    ///         None)
    ///     .try_into()?;
    /// # Ok(())
    /// # }
    pub fn setup<'a>(
        pieces: impl IntoIterator<Item = &'a (Square, Piece, Color)>,
        side_to_move: Color,
        white_castle_rights: CastleRights,
        black_castle_rights: CastleRights,
        en_passant: Option<File>,
    ) -> BoardBuilder {
        let mut result = BoardBuilder {
            pieces: [None; 64],
            side_to_move: side_to_move,
            castle_rights: [white_castle_rights, black_castle_rights],
            en_passant: en_passant,
            reserves: [Reserve::new(); NUM_COLORS],
            promoted: EMPTY,
        };

        for piece in pieces.into_iter() {
            result.pieces[piece.0.to_index()] = Some((piece.1, piece.2));
        }

        result
    }

    /// Get the current player
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Board, Color};
    ///
    /// let bb: BoardBuilder = Board::default().into();
    /// assert_eq!(bb.get_side_to_move(), Color::White);
    /// ```
    pub fn get_side_to_move(&self) -> Color {
        self.side_to_move
    }

    /// Get the castle rights for a player
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Board, CastleRights, Color};
    ///
    /// let bb: BoardBuilder = Board::default().into();
    /// assert_eq!(bb.get_castle_rights(Color::White), CastleRights::Both);
    /// ```
    pub fn get_castle_rights(&self, color: Color) -> CastleRights {
        self.castle_rights[color.to_index()]
    }

    /// Get the current en_passant square
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Board, Square, ChessMove};
    ///
    /// let board = Board::default()
    ///     .make_move_new(ChessMove::new(Square::E2, Square::E4, None))
    ///     .make_move_new(ChessMove::new(Square::H7, Square::H6, None))
    ///     .make_move_new(ChessMove::new(Square::E4, Square::E5, None))
    ///     .make_move_new(ChessMove::new(Square::D7, Square::D5, None));
    /// let bb: BoardBuilder = board.into();
    /// assert_eq!(bb.get_en_passant(), Some(Square::D5));
    /// ```
    pub fn get_en_passant(&self) -> Option<Square> {
        self.en_passant
            .map(|f| Square::make_square((!self.get_side_to_move()).to_fourth_rank(), f))
    }

    /// Set the side to move on the position
    ///
    /// This function can be used on self directly or in a builder pattern.
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Color};
    /// BoardBuilder::new()
    ///              .side_to_move(Color::Black);      
    ///
    /// let mut bb = BoardBuilder::new();
    /// bb.side_to_move(Color::Black);
    /// ```
    pub fn side_to_move<'a>(&'a mut self, color: Color) -> &'a mut Self {
        self.side_to_move = color;
        self
    }

    /// Set the castle rights for a particular color on the position
    ///
    /// This function can be used on self directly or in a builder pattern.
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Color, CastleRights};
    /// BoardBuilder::new()
    ///              .castle_rights(Color::White, CastleRights::NoRights);
    ///
    /// let mut bb = BoardBuilder::new();
    /// bb.castle_rights(Color::Black, CastleRights::Both);
    /// ```
    pub fn castle_rights<'a>(
        &'a mut self,
        color: Color,
        castle_rights: CastleRights,
    ) -> &'a mut Self {
        self.castle_rights[color.to_index()] = castle_rights;
        self
    }

    /// Set a piece on a square.
    ///
    /// Note that this can and will overwrite another piece on the square if need.
    ///
    /// Note also that this will not update your castle rights.
    ///
    /// This function can be used on self directly or in a builder pattern.
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Color, Square, Piece};
    ///
    /// BoardBuilder::new()
    ///              .piece(Square::A1, Piece::Rook, Color::White);
    ///
    /// let mut bb = BoardBuilder::new();
    /// bb.piece(Square::A8, Piece::Rook, Color::Black);
    /// ```
    pub fn piece<'a>(&'a mut self, square: Square, piece: Piece, color: Color) -> &'a mut Self {
        self[square] = Some((piece, color));
        self
    }

    /// Clear a square on the board.
    ///
    /// Note that this will not update your castle rights.
    ///
    /// This function can be used on self directly or in a builder pattern.
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Square, Board};
    ///
    /// let mut bb: BoardBuilder = Board::default().into();
    /// bb.clear_square(Square::A1);
    /// ```
    pub fn clear_square<'a>(&'a mut self, square: Square) -> &'a mut Self {
        self[square] = None;
        self
    }

    /// Set or clear the en_passant `File`.
    ///
    /// This function can be used directly or in a builder pattern.
    ///
    /// ```
    /// use bughouse_chess::{BoardBuilder, Square, Board, File, Color, Piece};
    ///
    /// BoardBuilder::new()
    ///              .piece(Square::E4, Piece::Pawn, Color::White)
    ///              .en_passant(Some(File::E));
    /// ```
    pub fn en_passant<'a>(&'a mut self, file: Option<File>) -> &'a mut Self {
        self.en_passant = file;
        self
    }

    /// Get the reserves for both colors.
    pub fn get_reserves(&self) -> [Reserve; NUM_COLORS] {
        self.reserves
    }

    /// Get the promoted bitboard.
    pub fn get_promoted(&self) -> BitBoard {
        self.promoted
    }

    /// Set the reserves for a particular color.
    pub fn set_reserves<'a>(&'a mut self, color: Color, reserve: Reserve) -> &'a mut Self {
        self.reserves[color.to_index()] = reserve;
        self
    }

    /// Mark a square as containing a promoted piece.
    pub fn mark_promoted<'a>(&'a mut self, square: Square) -> &'a mut Self {
        self.promoted |= BitBoard::from_square(square);
        self
    }
}

impl Index<Square> for BoardBuilder {
    type Output = Option<(Piece, Color)>;

    fn index<'a>(&'a self, index: Square) -> &'a Self::Output {
        &self.pieces[index.to_index()]
    }
}

impl IndexMut<Square> for BoardBuilder {
    fn index_mut<'a>(&'a mut self, index: Square) -> &'a mut Self::Output {
        &mut self.pieces[index.to_index()]
    }
}

impl fmt::Display for BoardBuilder {
    /// Format as BFEN (Bughouse FEN).
    ///
    /// Emits `[reserves]` after piece placement, and `~` after promoted pieces.
    /// If reserves are empty and no promoted pieces exist, output is compatible
    /// with standard FEN (except for the `[]` empty reserves marker).
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut count = 0;
        for rank in ALL_RANKS.iter().rev() {
            for file in ALL_FILES.iter() {
                let sq = Square::make_square(*rank, *file);
                let square = sq.to_index();

                if self.pieces[square].is_some() && count != 0 {
                    write!(f, "{}", count)?;
                    count = 0;
                }

                if let Some((piece, color)) = self.pieces[square] {
                    write!(f, "{}", piece.to_string(color))?;
                    // Emit ~ for promoted pieces
                    if self.promoted & BitBoard::from_square(sq) != EMPTY {
                        write!(f, "~")?;
                    }
                } else {
                    count += 1;
                }
            }

            if count != 0 {
                write!(f, "{}", count)?;
            }

            if *rank != Rank::First {
                write!(f, "/")?;
            }
            count = 0;
        }

        // Emit reserves in canonical order: QRBNPqrbnp
        write!(f, "[")?;
        let white_reserve = &self.reserves[Color::White.to_index()];
        let black_reserve = &self.reserves[Color::Black.to_index()];
        for (piece, ch) in [
            (Piece::Queen, 'Q'), (Piece::Rook, 'R'), (Piece::Bishop, 'B'),
            (Piece::Knight, 'N'), (Piece::Pawn, 'P'),
        ] {
            for _ in 0..white_reserve.count(piece) {
                write!(f, "{}", ch)?;
            }
        }
        for (piece, ch) in [
            (Piece::Queen, 'q'), (Piece::Rook, 'r'), (Piece::Bishop, 'b'),
            (Piece::Knight, 'n'), (Piece::Pawn, 'p'),
        ] {
            for _ in 0..black_reserve.count(piece) {
                write!(f, "{}", ch)?;
            }
        }
        write!(f, "]")?;

        write!(f, " ")?;

        if self.side_to_move == Color::White {
            write!(f, "w ")?;
        } else {
            write!(f, "b ")?;
        }

        write!(
            f,
            "{}",
            self.castle_rights[Color::White.to_index()].to_string(Color::White)
        )?;
        write!(
            f,
            "{}",
            self.castle_rights[Color::Black.to_index()].to_string(Color::Black)
        )?;
        if self.castle_rights[0] == CastleRights::NoRights
            && self.castle_rights[1] == CastleRights::NoRights
        {
            write!(f, "-")?;
        }

        write!(f, " ")?;
        if let Some(sq) = self.get_en_passant() {
            write!(f, "{}", sq)?;
        } else {
            write!(f, "-")?;
        }

        write!(f, " 0 1")
    }
}

impl Default for BoardBuilder {
    fn default() -> BoardBuilder {
        BoardBuilder::from_str("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[] w KQkq - 0 1").unwrap()
    }
}

impl FromStr for BoardBuilder {
    type Err = Error;

    /// Parse a BFEN (Bughouse FEN) string.
    ///
    /// BFEN extends standard FEN with:
    /// - `[reserves]` after piece placement (e.g., `[QNPqp]` = white has Q+N+P, black has q+p)
    /// - `~` suffix on promoted pieces (e.g., `Q~` marks a promoted queen)
    ///
    /// Standard FEN strings (without brackets) are also accepted.
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut cur_rank = Rank::Eighth;
        let mut cur_file = File::A;
        let mut fen = &mut BoardBuilder::new();

        let tokens: Vec<&str> = value.split(' ').collect();
        if tokens.len() < 4 {
            return Err(Error::InvalidFen {
                fen: value.to_string(),
            });
        }

        let pieces_and_reserves = tokens[0];
        let side = tokens[1];
        let castles = tokens[2];
        let ep = tokens[3];

        // Split piece placement from reserves bracket notation
        // Format: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[QNPqp]"
        let (pieces_str, reserves_str) = if let Some(bracket_start) = pieces_and_reserves.find('[') {
            let pieces = &pieces_and_reserves[..bracket_start];
            let rest = &pieces_and_reserves[bracket_start..];
            if let Some(bracket_end) = rest.find(']') {
                (pieces, Some(&rest[1..bracket_end]))
            } else {
                return Err(Error::InvalidFen { fen: value.to_string() });
            }
        } else {
            (pieces_and_reserves, None)
        };

        // Parse piece placement with ~ promoted marker support
        let mut chars = pieces_str.chars().peekable();
        while let Some(x) = chars.next() {
            let (piece, color) = match x {
                '/' => {
                    cur_rank = cur_rank.down();
                    cur_file = File::A;
                    continue;
                }
                '1'..='8' => {
                    cur_file =
                        File::from_index(cur_file.to_index() + (x as usize) - ('0' as usize));
                    continue;
                }
                'r' => (Piece::Rook, Color::Black),
                'R' => (Piece::Rook, Color::White),
                'n' => (Piece::Knight, Color::Black),
                'N' => (Piece::Knight, Color::White),
                'b' => (Piece::Bishop, Color::Black),
                'B' => (Piece::Bishop, Color::White),
                'p' => (Piece::Pawn, Color::Black),
                'P' => (Piece::Pawn, Color::White),
                'q' => (Piece::Queen, Color::Black),
                'Q' => (Piece::Queen, Color::White),
                'k' => (Piece::King, Color::Black),
                'K' => (Piece::King, Color::White),
                _ => {
                    return Err(Error::InvalidFen {
                        fen: value.to_string(),
                    });
                }
            };

            let sq = Square::make_square(cur_rank, cur_file);
            fen[sq] = Some((piece, color));

            // Check for ~ promoted marker
            if chars.peek() == Some(&'~') {
                chars.next(); // consume the ~
                fen.promoted |= BitBoard::from_square(sq);
            }

            cur_file = cur_file.right();
        }

        // Parse reserves
        if let Some(res_str) = reserves_str {
            for c in res_str.chars() {
                let (piece, color) = match c {
                    'Q' => (Piece::Queen, Color::White),
                    'R' => (Piece::Rook, Color::White),
                    'B' => (Piece::Bishop, Color::White),
                    'N' => (Piece::Knight, Color::White),
                    'P' => (Piece::Pawn, Color::White),
                    'q' => (Piece::Queen, Color::Black),
                    'r' => (Piece::Rook, Color::Black),
                    'b' => (Piece::Bishop, Color::Black),
                    'n' => (Piece::Knight, Color::Black),
                    'p' => (Piece::Pawn, Color::Black),
                    _ => {
                        return Err(Error::InvalidFen { fen: value.to_string() });
                    }
                };
                fen.reserves[color.to_index()].add(piece);
            }
        }

        match side {
            "w" | "W" => fen = fen.side_to_move(Color::White),
            "b" | "B" => fen = fen.side_to_move(Color::Black),
            _ => {
                return Err(Error::InvalidFen {
                    fen: value.to_string(),
                })
            }
        }

        if castles.contains("K") && castles.contains("Q") {
            fen.castle_rights[Color::White.to_index()] = CastleRights::Both;
        } else if castles.contains("K") {
            fen.castle_rights[Color::White.to_index()] = CastleRights::KingSide;
        } else if castles.contains("Q") {
            fen.castle_rights[Color::White.to_index()] = CastleRights::QueenSide;
        } else {
            fen.castle_rights[Color::White.to_index()] = CastleRights::NoRights;
        }

        if castles.contains("k") && castles.contains("q") {
            fen.castle_rights[Color::Black.to_index()] = CastleRights::Both;
        } else if castles.contains("k") {
            fen.castle_rights[Color::Black.to_index()] = CastleRights::KingSide;
        } else if castles.contains("q") {
            fen.castle_rights[Color::Black.to_index()] = CastleRights::QueenSide;
        } else {
            fen.castle_rights[Color::Black.to_index()] = CastleRights::NoRights;
        }

        if let Ok(sq) = Square::from_str(&ep) {
            fen = fen.en_passant(Some(sq.get_file()));
        }

        Ok(*fen)
    }
}

impl From<&Board> for BoardBuilder {
    fn from(board: &Board) -> Self {
        let mut pieces = vec![];
        for sq in ALL_SQUARES.iter() {
            if let Some(piece) = board.piece_on(*sq) {
                let color = board.color_on(*sq).unwrap();
                pieces.push((*sq, piece, color));
            }
        }

        let mut builder = BoardBuilder::setup(
            &pieces,
            board.side_to_move(),
            board.castle_rights(Color::White),
            board.castle_rights(Color::Black),
            board.en_passant().map(|sq| sq.get_file()),
        );
        builder.reserves = [*board.reserves(Color::White), *board.reserves(Color::Black)];
        builder.promoted = *board.promoted();
        builder
    }
}

impl From<Board> for BoardBuilder {
    fn from(board: Board) -> Self {
        (&board).into()
    }
}

#[cfg(test)]
use std::convert::TryInto;

#[test]
fn check_initial_position() {
    let initial_bfen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[] w KQkq - 0 1";
    let fen: BoardBuilder = Board::default().into();
    let computed_initial_fen = format!("{}", fen);
    assert_eq!(computed_initial_fen, initial_bfen);

    let pass_through = format!("{}", BoardBuilder::default());
    assert_eq!(pass_through, initial_bfen);
}

#[test]
fn invalid_castle_rights() {
    let res: Result<Board, _> = BoardBuilder::new()
        .piece(Square::A1, Piece::King, Color::White)
        .piece(Square::A8, Piece::King, Color::Black)
        .castle_rights(Color::White, CastleRights::Both)
        .try_into();
    assert!(res.is_err());
}

#[test]
fn test_kissing_kings() {
    // In bughouse, kings CAN be adjacent (king can move next to enemy king).
    let res: Result<Board, _> = BoardBuilder::new()
        .piece(Square::A1, Piece::King, Color::White)
        .piece(Square::A2, Piece::King, Color::Black)
        .try_into();
    assert!(res.is_ok());
}

#[test]
fn test_in_check() {
    let mut bb: BoardBuilder = BoardBuilder::new();
    bb.piece(Square::A1, Piece::King, Color::White)
        .piece(Square::A8, Piece::King, Color::Black)
        .piece(Square::H1, Piece::Rook, Color::Black);

    let board: Board = (&bb).try_into().unwrap();
    assert_eq!(*board.checkers(), BitBoard::from_square(Square::H1));

    // In bughouse, opponent CAN be in check when it's your move
    // (you chose not to escape check, and now the opponent can capture your king).
    bb.side_to_move(Color::Black);
    let res: Result<Board, _> = bb.try_into();
    assert!(res.is_ok());
}
