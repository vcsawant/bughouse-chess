use crate::bitboard::{BitBoard, EMPTY};
use crate::board::Board;
use crate::chess_move::ChessMove;
use crate::magic::get_rank;
use crate::movegen::piece_type::*;
use crate::piece::{Piece, NUM_PROMOTION_PIECES, PROMOTION_PIECES};
use crate::square::Square;
use arrayvec::ArrayVec;
use nodrop::NoDrop;
use std::iter::ExactSizeIterator;
use std::mem;

#[derive(Copy, Clone, PartialEq, PartialOrd)]
pub struct SquareAndBitBoard {
    square: Square,
    bitboard: BitBoard,
    promotion: bool,
}

impl SquareAndBitBoard {
    pub fn new(sq: Square, bb: BitBoard, promotion: bool) -> SquareAndBitBoard {
        SquareAndBitBoard {
            square: sq,
            bitboard: bb,
            promotion: promotion,
        }
    }
}

pub type MoveList = NoDrop<ArrayVec<SquareAndBitBoard, 18>>;

/// An incremental move generator
///
/// This structure enumerates moves slightly slower than board.enumerate_moves(...),
/// but has some extra features, such as:
///
/// * Being an iterator
/// * Not requiring you to create a buffer
/// * Only iterating moves that match a certain pattern
/// * Being iterable multiple times (such as, iterating once for all captures, then iterating again
///   for all quiets)
/// * Doing as little work early on as possible, so that if you are not going to look at every move, the
///   struture moves faster
/// * Being able to iterate pseudo legal moves, while keeping the (nearly) free legality checks in
///   place
///
/// # Examples
///
/// ```
/// use bughouse_chess::MoveGen;
/// use bughouse_chess::Board;
/// use bughouse_chess::EMPTY;
/// use bughouse_chess::construct;
///
/// // create a board with the initial position
/// let board = Board::default();
///
/// // create an iterable
/// let mut iterable = MoveGen::new_legal(&board);
///
/// // make sure .len() works.
/// assert_eq!(iterable.len(), 20); // the .len() function does *not* consume the iterator
///
/// // lets iterate over targets.
/// let targets = board.color_combined(!board.side_to_move());
/// iterable.set_iterator_mask(*targets);
///
/// // count the number of targets
/// let mut count = 0;
/// for _ in &mut iterable {
///     count += 1;
///     // This move captures one of my opponents pieces (with the exception of en passant)
/// }
///
/// // now, iterate over the rest of the moves
/// iterable.set_iterator_mask(!EMPTY);
/// for _ in &mut iterable {
///     count += 1;
///     // This move does not capture anything
/// }
///
/// // make sure it works
/// assert_eq!(count, 20);
///
/// ```
pub struct MoveGen {
    moves: MoveList,
    promotion_index: usize,
    iterator_mask: BitBoard,
    index: usize,
}

impl MoveGen {
    /// Generate all pseudo-legal moves for bughouse.
    ///
    /// In bughouse, there is no check/pin filtering — all pieces can move freely.
    /// The only restriction is that pieces can't land on friendly pieces (handled by mask).
    #[inline(always)]
    fn enumerate_moves(board: &Board) -> MoveList {
        let mask = !board.color_combined(board.side_to_move());
        let mut movelist = NoDrop::new(ArrayVec::<SquareAndBitBoard, 18>::new());

        PawnType::legals::<NotInCheckType>(&mut movelist, &board, mask);
        KnightType::legals::<NotInCheckType>(&mut movelist, &board, mask);
        BishopType::legals::<NotInCheckType>(&mut movelist, &board, mask);
        RookType::legals::<NotInCheckType>(&mut movelist, &board, mask);
        QueenType::legals::<NotInCheckType>(&mut movelist, &board, mask);
        KingType::legals::<NotInCheckType>(&mut movelist, &board, mask);

        movelist
    }

    /// Create a new `MoveGen` structure, only generating legal moves
    #[inline(always)]
    pub fn new_legal(board: &Board) -> MoveGen {
        MoveGen {
            moves: MoveGen::enumerate_moves(board),
            promotion_index: 0,
            iterator_mask: !EMPTY,
            index: 0,
        }
    }

    /// Never, ever, iterate any moves that land on the following squares
    pub fn remove_mask(&mut self, mask: BitBoard) {
        for x in 0..self.moves.len() {
            self.moves[x].bitboard &= !mask;
        }
    }

    /// Never, ever, iterate this move
    pub fn remove_move(&mut self, chess_move: ChessMove) -> bool {
        for x in 0..self.moves.len() {
            if self.moves[x].square == chess_move.get_source() {
                self.moves[x].bitboard &= !BitBoard::from_square(chess_move.get_dest());
                return true;
            }
        }
        false
    }

    /// For now, Only iterate moves that land on the following squares
    /// Note: Once iteration is completed, you can pass in a mask of ! `EMPTY`
    ///       to get the remaining moves, or another mask
    pub fn set_iterator_mask(&mut self, mask: BitBoard) {
        self.iterator_mask = mask;
        self.index = 0;

        // the iterator portion of this struct relies on the invariant that
        // the bitboards at the beginning of the moves[] array are the only
        // ones used.  As a result, we must partition the list such that the
        // assumption is true.

        // first, find the first non-used moves index, and store that in i
        let mut i = 0;
        while i < self.moves.len() && self.moves[i].bitboard & self.iterator_mask != EMPTY {
            i += 1;
        }

        // next, find each element past i where the moves are used, and store
        // that in i.  Then, increment i to point to a new unused slot.
        for j in (i + 1)..self.moves.len() {
            if self.moves[j].bitboard & self.iterator_mask != EMPTY {
                let backup = self.moves[i];
                self.moves[i] = self.moves[j];
                self.moves[j] = backup;
                i += 1;
            }
        }
    }

    /// In bughouse, all generated moves are automatically legal.
    ///
    /// There are no pin or check restrictions, and the king can move anywhere
    /// not occupied by friendly pieces.
    pub fn legal_quick(_board: &Board, _chess_move: ChessMove) -> bool {
        true
    }

    /// Generate all legal drop moves for the current position.
    ///
    /// A drop is placing a piece from reserves onto an empty square.
    /// Pawns cannot be dropped on rank 1 or 8.
    pub fn drop_moves(board: &Board) -> Vec<crate::bughouse_move::BughouseMove> {
        let empty_squares = !board.combined();
        let color = board.side_to_move();
        let reserve = board.reserves(color);
        let mut drops = Vec::new();

        let pawn_mask = empty_squares
            & !get_rank(crate::rank::Rank::First)
            & !get_rank(crate::rank::Rank::Eighth);

        for (piece, count) in reserve.iter() {
            if count == 0 {
                continue;
            }
            let targets = if piece == Piece::Pawn {
                pawn_mask
            } else {
                empty_squares
            };
            for sq in targets {
                drops.push(crate::bughouse_move::BughouseMove::Drop { piece, square: sq });
            }
        }
        drops
    }

    /// Fastest perft test with this structure
    pub fn movegen_perft_test(board: &Board, depth: usize) -> usize {
        let iterable = MoveGen::new_legal(board);

        let mut result: usize = 0;
        if depth == 1 {
            iterable.len()
        } else {
            for m in iterable {
                let bresult = board.make_move_new(m);
                result += MoveGen::movegen_perft_test(&bresult, depth - 1);
            }
            result
        }
    }

    #[cfg(test)]
    /// Do a perft test after splitting the moves up into two groups
    pub fn movegen_perft_test_piecewise(board: &Board, depth: usize) -> usize {
        let mut iterable = MoveGen::new_legal(board);

        let targets = board.color_combined(!board.side_to_move());
        let mut result: usize = 0;

        if depth == 1 {
            iterable.set_iterator_mask(*targets);
            result += iterable.len();
            iterable.set_iterator_mask(!targets);
            result += iterable.len();
            result
        } else {
            iterable.set_iterator_mask(*targets);
            for x in &mut iterable {
                let mut bresult = mem::MaybeUninit::<Board>::uninit();
                unsafe {
                    board.make_move(x, &mut *bresult.as_mut_ptr());
                    result += MoveGen::movegen_perft_test(&*bresult.as_ptr(), depth - 1);
                }
            }
            iterable.set_iterator_mask(!EMPTY);
            for x in &mut iterable {
                let mut bresult = mem::MaybeUninit::<Board>::uninit();
                unsafe {
                    board.make_move(x, &mut *bresult.as_mut_ptr());
                    result += MoveGen::movegen_perft_test(&*bresult.as_ptr(), depth - 1);
                }
            }
            result
        }
    }
}

impl ExactSizeIterator for MoveGen {
    /// Give the exact length of this iterator
    fn len(&self) -> usize {
        let mut result = 0;
        for i in 0..self.moves.len() {
            if self.moves[i].bitboard & self.iterator_mask == EMPTY {
                break;
            }
            if self.moves[i].promotion {
                result += ((self.moves[i].bitboard & self.iterator_mask).popcnt() as usize)
                    * NUM_PROMOTION_PIECES;
            } else {
                result += (self.moves[i].bitboard & self.iterator_mask).popcnt() as usize;
            }
        }
        result
    }
}

impl Iterator for MoveGen {
    type Item = ChessMove;

    /// Give a size_hint to some functions that need it
    fn size_hint(&self) -> (usize, Option<usize>) {
        let len = self.len();
        (len, Some(len))
    }

    /// Find the next chess move.
    fn next(&mut self) -> Option<ChessMove> {
        if self.index >= self.moves.len()
            || self.moves[self.index].bitboard & self.iterator_mask == EMPTY
        {
            // are we done?
            None
        } else if self.moves[self.index].promotion {
            let moves = &mut self.moves[self.index];

            let dest = (moves.bitboard & self.iterator_mask).to_square();

            // deal with potential promotions for this pawn
            let result = ChessMove::new(
                moves.square,
                dest,
                Some(PROMOTION_PIECES[self.promotion_index]),
            );
            self.promotion_index += 1;
            if self.promotion_index >= NUM_PROMOTION_PIECES {
                moves.bitboard ^= BitBoard::from_square(dest);
                self.promotion_index = 0;
                if moves.bitboard & self.iterator_mask == EMPTY {
                    self.index += 1;
                }
            }
            Some(result)
        } else {
            // not a promotion move, so its a 'normal' move as far as this function is concerned
            let moves = &mut self.moves[self.index];
            let dest = (moves.bitboard & self.iterator_mask).to_square();

            moves.bitboard ^= BitBoard::from_square(dest);
            if moves.bitboard & self.iterator_mask == EMPTY {
                self.index += 1;
            }
            Some(ChessMove::new(moves.square, dest, None))
        }
    }
}

#[cfg(test)]
use crate::board::BoardStatus;
#[cfg(test)]
use crate::board_builder::BoardBuilder;
#[cfg(test)]
use crate::bughouse_move::BughouseMove;
#[cfg(test)]
use crate::castle_rights::CastleRights;
#[cfg(test)]
use crate::color::Color;
#[cfg(test)]
use crate::rank::Rank;
#[cfg(test)]
use std::collections::HashSet;
#[cfg(test)]
use std::convert::TryInto;
#[cfg(test)]
use std::str::FromStr;

#[cfg(test)]
fn move_of(m: &str) -> ChessMove {
    let promo = if m.len() > 4 {
        Some(match m.as_bytes()[4] {
            b'q' => Piece::Queen,
            b'r' => Piece::Rook,
            b'b' => Piece::Bishop,
            b'n' => Piece::Knight,
            _ => panic!("unrecognized uci move: {}", m),
        })
    } else {
        None
    };
    ChessMove::new(
        Square::from_str(&m[..2]).unwrap(),
        Square::from_str(&m[2..4]).unwrap(),
        promo,
    )
}

// ============================================================
// Bughouse-specific move generation tests
// ============================================================

#[test]
fn test_starting_position_moves() {
    // Standard starting position should still have 20 legal moves in bughouse.
    let board = Board::default();
    let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
    assert_eq!(moves.len(), 20);
}

#[test]
fn test_king_can_move_into_check() {
    // In bughouse, the king is free to move to an attacked square.
    // White king on e1, black rook on h2 attacks e2. King should be able to move to e2.
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::E8, Piece::King, Color::Black)
        .piece(Square::H2, Piece::Rook, Color::Black)
        .try_into()
        .unwrap();

    let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
    let dests: HashSet<Square> = moves.iter().map(|m| m.get_dest()).collect();

    // e2 is attacked by rook on h2, but king should still be able to go there
    assert!(dests.contains(&Square::E2));
    // d1, d2 also attacked by rook, king should still go there
    assert!(dests.contains(&Square::D1));
    assert!(dests.contains(&Square::D2));
    assert!(dests.contains(&Square::F1));
    assert!(dests.contains(&Square::F2));
}

#[test]
fn test_pieces_can_move_while_pinned() {
    // In bughouse, a pinned piece can freely move (exposing the king).
    // White king e1, white bishop e4, black rook e8. Bishop is "pinned" on e-file.
    // In standard chess, bishop on e4 can't move off e-file. In bughouse, it can.
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::E4, Piece::Bishop, Color::White)
        .piece(Square::E8, Piece::King, Color::Black)
        .piece(Square::A8, Piece::Rook, Color::Black)
        .try_into()
        .unwrap();

    let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
    let bishop_moves: Vec<&ChessMove> = moves
        .iter()
        .filter(|m| m.get_source() == Square::E4)
        .collect();

    // Bishop should have all diagonal moves (not restricted to e-file pin line)
    let bishop_dests: HashSet<Square> = bishop_moves.iter().map(|m| m.get_dest()).collect();
    assert!(bishop_dests.contains(&Square::D5)); // off the e-file
    assert!(bishop_dests.contains(&Square::F3)); // off the e-file
    assert!(bishop_dests.contains(&Square::D3)); // off the e-file
}

#[test]
fn test_all_pieces_move_in_double_check() {
    // In standard chess, only king can move in double check.
    // In bughouse, ALL pieces can move regardless of checks.
    // White king e1, white knight g1 — black queen attacks via a5-e1 diagonal,
    // black rook attacks via e8-e1 file = double check on white king.
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::G1, Piece::Knight, Color::White)
        .piece(Square::E8, Piece::Rook, Color::Black)
        .piece(Square::A5, Piece::Queen, Color::Black)
        .piece(Square::H8, Piece::King, Color::Black)
        .try_into()
        .unwrap();

    let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();

    // Knight on g1 should have moves despite being in double check
    let knight_moves: Vec<&ChessMove> = moves
        .iter()
        .filter(|m| m.get_source() == Square::G1)
        .collect();
    assert!(!knight_moves.is_empty(), "Knight should be able to move in double check (bughouse rules)");
}

#[test]
fn test_castling_through_check() {
    // In bughouse, castling through attacked squares is allowed.
    // White king e1, white rook h1, black rook on f8 attacks f1 (between king and rook).
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::H1, Piece::Rook, Color::White)
        .piece(Square::F8, Piece::Rook, Color::Black)
        .piece(Square::A8, Piece::King, Color::Black)
        .castle_rights(Color::White, CastleRights::KingSide)
        .castle_rights(Color::Black, CastleRights::NoRights)
        .try_into()
        .unwrap();

    let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
    let king_dests: HashSet<Square> = moves
        .iter()
        .filter(|m| m.get_source() == Square::E1)
        .map(|m| m.get_dest())
        .collect();

    // g1 is the castling destination — should be allowed even though f1 is attacked
    assert!(king_dests.contains(&Square::G1), "Kingside castling through attacked f1 should be allowed in bughouse");
}

#[test]
fn test_castling_out_of_check() {
    // In bughouse, you can castle even while in check.
    // White king e1 in check from black rook on e8. White rook on h1.
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::H1, Piece::Rook, Color::White)
        .piece(Square::E8, Piece::Rook, Color::Black)
        .piece(Square::A8, Piece::King, Color::Black)
        .castle_rights(Color::White, CastleRights::KingSide)
        .castle_rights(Color::Black, CastleRights::NoRights)
        .try_into()
        .unwrap();

    let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
    let king_dests: HashSet<Square> = moves
        .iter()
        .filter(|m| m.get_source() == Square::E1)
        .map(|m| m.get_dest())
        .collect();

    assert!(king_dests.contains(&Square::G1), "Castling out of check should be allowed in bughouse");
}

#[test]
fn test_king_capturable_detection() {
    // When the side to move can capture the opponent's king, status = KingCapturable.
    // White to move, black king on e8, white rook on e1 = black king capturable? No — rook
    // doesn't directly attack king (pieces in between). Let's make it simpler.
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::E7, Piece::Rook, Color::White)
        .piece(Square::E8, Piece::King, Color::Black)
        .side_to_move(Color::White)
        .try_into()
        .unwrap();

    assert_eq!(board.status(), BoardStatus::KingCapturable);
}

#[test]
fn test_ongoing_status() {
    let board = Board::default();
    assert_eq!(board.status(), BoardStatus::Ongoing);
}

#[test]
fn test_drop_moves_empty_reserves() {
    // No reserves = no drop moves.
    let board = Board::default();
    let drops = MoveGen::drop_moves(&board);
    assert_eq!(drops.len(), 0);
}

#[test]
fn test_drop_moves_with_reserves() {
    // Add a knight to white's reserves. In starting position, there are 32 empty squares.
    // A knight can be dropped on any of them.
    let mut board = Board::default();
    board.add_to_reserve(Color::White, Piece::Knight);

    let drops = MoveGen::drop_moves(&board);
    assert_eq!(drops.len(), 32); // 32 empty squares, 1 piece type
}

#[test]
fn test_pawn_drop_rank_restriction() {
    // Pawns cannot be dropped on rank 1 or rank 8.
    // On starting position with only a pawn in reserve:
    // 32 empty squares total, but ranks 1 and 8 are fully occupied, so all 32 are valid.
    // Let's use a position with empty first/last rank squares.
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::E8, Piece::King, Color::Black)
        .try_into()
        .unwrap();

    // Board has 62 empty squares. Rank 1 has 7 empty, rank 8 has 7 empty = 14 restricted.
    // Pawns can drop on 62 - 14 = 48 squares.
    let mut board_with_pawn = board;
    board_with_pawn.add_to_reserve(Color::White, Piece::Pawn);

    let drops = MoveGen::drop_moves(&board_with_pawn);
    let pawn_drops: Vec<&BughouseMove> = drops
        .iter()
        .filter(|m| matches!(m, BughouseMove::Drop { piece: Piece::Pawn, .. }))
        .collect();

    assert_eq!(pawn_drops.len(), 48);

    // Verify no pawn drops on rank 1 or 8
    for d in &pawn_drops {
        if let BughouseMove::Drop { square, .. } = d {
            assert_ne!(square.get_rank(), Rank::First, "Pawn dropped on rank 1");
            assert_ne!(square.get_rank(), Rank::Eighth, "Pawn dropped on rank 8");
        }
    }
}

#[test]
fn test_drop_move_execution() {
    // Drop a knight onto an empty square and verify it appears on the board.
    let mut board = Board::default();
    board.add_to_reserve(Color::White, Piece::Knight);

    let new_board = board.make_drop_new(Piece::Knight, Square::E4);
    assert!(new_board.is_some());

    let new_board = new_board.unwrap();
    // Knight should be on e4
    assert_ne!(new_board.pieces(Piece::Knight) & BitBoard::from_square(Square::E4), EMPTY);
    // Reserve should be empty
    assert_eq!(new_board.reserves(Color::White).count(Piece::Knight), 0);
    // Side to move should flip
    assert_eq!(new_board.side_to_move(), Color::Black);
}

#[test]
fn test_drop_on_occupied_square_fails() {
    // Can't drop on an occupied square.
    let mut board = Board::default();
    board.add_to_reserve(Color::White, Piece::Knight);

    // e2 has a white pawn
    let result = board.make_drop_new(Piece::Knight, Square::E2);
    assert!(result.is_none());
}

#[test]
fn test_drop_without_reserve_fails() {
    // Can't drop a piece you don't have.
    let board = Board::default();
    let result = board.make_drop_new(Piece::Knight, Square::E4);
    assert!(result.is_none());
}

#[test]
fn test_pawn_drop_on_back_rank_fails() {
    let board: Board = BoardBuilder::new()
        .piece(Square::E1, Piece::King, Color::White)
        .piece(Square::E8, Piece::King, Color::Black)
        .try_into()
        .unwrap();

    let mut board = board;
    board.add_to_reserve(Color::White, Piece::Pawn);

    // Can't drop pawn on rank 1
    assert!(board.make_drop_new(Piece::Pawn, Square::A1).is_none());
    // Can't drop pawn on rank 8
    assert!(board.make_drop_new(Piece::Pawn, Square::A8).is_none());
    // Can drop pawn on rank 4
    assert!(board.make_drop_new(Piece::Pawn, Square::A4).is_some());
}

#[test]
fn test_capture_tracking() {
    // Make a capture and verify the captured piece info.
    // Play 1. e4 d5 2. exd5 — white captures black pawn on d5.
    let board = Board::default();
    let board = board.make_move_new(move_of("e2e4"));
    let board = board.make_move_new(move_of("d7d5"));

    let (new_board, capture) = board.make_move_with_capture(move_of("e4d5"));
    assert!(capture.is_some());
    let (piece, promoted) = capture.unwrap();
    assert_eq!(piece, Piece::Pawn);
    assert!(!promoted);

    // Board should have the pawn on d5
    assert_ne!(
        new_board.pieces(Piece::Pawn) & BitBoard::from_square(Square::D5),
        EMPTY
    );
}

#[test]
fn test_bfen_roundtrip() {
    // Parse a BFEN with reserves, convert back to string, verify roundtrip.
    let bfen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[QNPqp] w KQkq - 0 1";
    let board: Board = Board::from_str(bfen).unwrap();

    assert_eq!(board.reserves(Color::White).count(Piece::Queen), 1);
    assert_eq!(board.reserves(Color::White).count(Piece::Knight), 1);
    assert_eq!(board.reserves(Color::White).count(Piece::Pawn), 1);
    assert_eq!(board.reserves(Color::Black).count(Piece::Queen), 1);
    assert_eq!(board.reserves(Color::Black).count(Piece::Pawn), 1);

    // Roundtrip through BoardBuilder
    let bb: BoardBuilder = board.into();
    let roundtrip = format!("{}", bb);
    assert_eq!(roundtrip, bfen);
}

#[test]
fn test_masked_move_gen() {
    // Test that iterator masking still works for capture-only generation.
    let board =
        Board::from_str("r1bqkb1r/pp3ppp/5n2/2ppn1N1/4pP2/1BN1P3/PPPP2PP/R1BQ1RK1 w kq - 0 9")
            .unwrap();

    let mut capture_moves = MoveGen::new_legal(&board);
    let targets = *board.color_combined(!board.side_to_move());
    capture_moves.set_iterator_mask(targets);

    let actual: HashSet<ChessMove> = capture_moves.collect();

    // In bughouse, more captures may be available since pinned pieces can capture.
    // Verify the standard captures are still present.
    let expected_captures = vec![
        move_of("f4e5"),
        move_of("b3d5"),
        move_of("g5e4"),
        move_of("g5f7"),
        move_of("g5h7"),
        move_of("c3e4"),
        move_of("c3d5"),
    ];

    for cap in &expected_captures {
        assert!(actual.contains(cap), "Expected capture {} not found", cap);
    }
}
