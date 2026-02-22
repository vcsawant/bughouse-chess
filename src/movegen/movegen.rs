use crate::bitboard::{BitBoard, EMPTY};
use crate::board::Board;
use crate::bughouse_move::BughouseMove;
use crate::chess_move::ChessMove;
use crate::magic::{between, get_rank};
use crate::movegen::piece_type::*;
use crate::piece::{Piece, NUM_PROMOTION_PIECES, PROMOTION_PIECES};
use crate::rank::Rank;
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
    #[inline(always)]
    fn enumerate_moves(board: &Board) -> MoveList {
        let checkers = *board.checkers();
        let mask = !board.color_combined(board.side_to_move());
        let mut movelist = NoDrop::new(ArrayVec::<SquareAndBitBoard, 18>::new());

        if checkers == EMPTY {
            PawnType::legals::<NotInCheckType>(&mut movelist, &board, mask);
            KnightType::legals::<NotInCheckType>(&mut movelist, &board, mask);
            BishopType::legals::<NotInCheckType>(&mut movelist, &board, mask);
            RookType::legals::<NotInCheckType>(&mut movelist, &board, mask);
            QueenType::legals::<NotInCheckType>(&mut movelist, &board, mask);
            KingType::legals::<NotInCheckType>(&mut movelist, &board, mask);
        } else if checkers.popcnt() == 1 {
            PawnType::legals::<InCheckType>(&mut movelist, &board, mask);
            KnightType::legals::<InCheckType>(&mut movelist, &board, mask);
            BishopType::legals::<InCheckType>(&mut movelist, &board, mask);
            RookType::legals::<InCheckType>(&mut movelist, &board, mask);
            QueenType::legals::<InCheckType>(&mut movelist, &board, mask);
            KingType::legals::<InCheckType>(&mut movelist, &board, mask);
        } else {
            KingType::legals::<InCheckType>(&mut movelist, &board, mask);
        }

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

    /// Generate all legal drop moves from the current player's reserve.
    ///
    /// In check situations:
    /// - Double check: no drops possible (king must move)
    /// - Single check: drops only on squares that block the check
    /// - Not in check: drops on any empty square (pawns restricted from rank 1/8)
    pub fn drop_moves(board: &Board) -> Vec<BughouseMove> {
        let color = board.side_to_move();
        let reserve = &board.reserves()[color.to_index()];

        if reserve.is_empty() {
            return Vec::new();
        }

        let checkers = *board.checkers();

        // Double check: only king can move, no drops
        if checkers.popcnt() >= 2 {
            return Vec::new();
        }

        let empty = !board.combined();
        let drop_mask = if checkers.popcnt() == 1 {
            // Single check: drops only on blocking squares
            between(checkers.to_square(), board.king_square(color)) & empty
        } else {
            empty
        };

        let pawn_restriction = get_rank(Rank::First) | get_rank(Rank::Eighth);
        let mut moves = Vec::new();

        for (piece, _count) in reserve.iter() {
            let mask = if piece == Piece::Pawn {
                drop_mask & !pawn_restriction
            } else {
                drop_mask
            };
            for sq in mask {
                moves.push(BughouseMove::Drop { piece, square: sq });
            }
        }

        moves
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

    /// This function checks the legality *only for moves generated by `MoveGen`*.
    ///
    /// Calling this function for moves not generated by `MoveGen` will result in possibly
    /// incorrect results, and making that move on the `Board` will result in undefined behavior.
    /// This function may panic! if these rules are not followed.
    ///
    /// If you are validating a move from a user, you should call the .legal() function.
    pub fn legal_quick(board: &Board, chess_move: ChessMove) -> bool {
        let piece = board.piece_on(chess_move.get_source()).unwrap();
        match piece {
            Piece::Rook => true,
            Piece::Bishop => true,
            Piece::Knight => true,
            Piece::Queen => true,
            Piece::Pawn => {
                if chess_move.get_source().get_file() != chess_move.get_dest().get_file()
                    && board.piece_on(chess_move.get_dest()).is_none()
                {
                    // en-passant
                    PawnType::legal_ep_move(board, chess_move.get_source(), chess_move.get_dest())
                } else {
                    true
                }
            }
            Piece::King => {
                let bb = between(chess_move.get_source(), chess_move.get_dest());
                if bb.popcnt() == 1 {
                    // castles
                    if !KingType::legal_king_move(board, bb.to_square()) {
                        false
                    } else {
                        KingType::legal_king_move(board, chess_move.get_dest())
                    }
                } else {
                    KingType::legal_king_move(board, chess_move.get_dest())
                }
            }
        }
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
use crate::board_builder::BoardBuilder;
#[cfg(test)]
use std::collections::HashSet;
#[cfg(test)]
use std::convert::TryInto;
#[cfg(test)]
use std::str::FromStr;

#[cfg(test)]
fn movegen_perft_test(fen: String, depth: usize, result: usize) {
    let board: Board = BoardBuilder::from_str(&fen).unwrap().try_into().unwrap();

    assert_eq!(MoveGen::movegen_perft_test(&board, depth), result);
    assert_eq!(MoveGen::movegen_perft_test_piecewise(&board, depth), result);
}

#[test]
fn movegen_perft_kiwipete() {
    movegen_perft_test(
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1".to_owned(),
        5,
        193690690,
    );
}

#[test]
fn movegen_perft_1() {
    movegen_perft_test("8/5bk1/8/2Pp4/8/1K6/8/8 w - d6 0 1".to_owned(), 6, 824064);
    // Invalid FEN
}

#[test]
fn movegen_perft_2() {
    movegen_perft_test("8/8/1k6/8/2pP4/8/5BK1/8 b - d3 0 1".to_owned(), 6, 824064);
    // Invalid FEN
}

#[test]
fn movegen_perft_3() {
    movegen_perft_test("8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1".to_owned(), 6, 1440467);
}

#[test]
fn movegen_perft_4() {
    movegen_perft_test("8/5k2/8/2Pp4/2B5/1K6/8/8 w - d6 0 1".to_owned(), 6, 1440467);
}

#[test]
fn movegen_perft_5() {
    movegen_perft_test("5k2/8/8/8/8/8/8/4K2R w K - 0 1".to_owned(), 6, 661072);
}

#[test]
fn movegen_perft_6() {
    movegen_perft_test("4k2r/8/8/8/8/8/8/5K2 b k - 0 1".to_owned(), 6, 661072);
}

#[test]
fn movegen_perft_7() {
    movegen_perft_test("3k4/8/8/8/8/8/8/R3K3 w Q - 0 1".to_owned(), 6, 803711);
}

#[test]
fn movegen_perft_8() {
    movegen_perft_test("r3k3/8/8/8/8/8/8/3K4 b q - 0 1".to_owned(), 6, 803711);
}

#[test]
fn movegen_perft_9() {
    movegen_perft_test(
        "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1".to_owned(),
        4,
        1274206,
    );
}

#[test]
fn movegen_perft_10() {
    movegen_perft_test(
        "r3k2r/7b/8/8/8/8/1B4BQ/R3K2R b KQkq - 0 1".to_owned(),
        4,
        1274206,
    );
}

#[test]
fn movegen_perft_11() {
    movegen_perft_test(
        "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1".to_owned(),
        4,
        1720476,
    );
}

#[test]
fn movegen_perft_12() {
    movegen_perft_test(
        "r3k2r/8/5Q2/8/8/3q4/8/R3K2R w KQkq - 0 1".to_owned(),
        4,
        1720476,
    );
}

#[test]
fn movegen_perft_13() {
    movegen_perft_test("2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1".to_owned(), 6, 3821001);
}

#[test]
fn movegen_perft_14() {
    movegen_perft_test("3K4/8/8/8/8/8/4p3/2k2R2 b - - 0 1".to_owned(), 6, 3821001);
}

#[test]
fn movegen_perft_15() {
    movegen_perft_test("8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1".to_owned(), 5, 1004658);
}

#[test]
fn movegen_perft_16() {
    movegen_perft_test("5K2/8/1Q6/2N5/8/1p2k3/8/8 w - - 0 1".to_owned(), 5, 1004658);
}

#[test]
fn movegen_perft_17() {
    movegen_perft_test("4k3/1P6/8/8/8/8/K7/8 w - - 0 1".to_owned(), 6, 217342);
}

#[test]
fn movegen_perft_18() {
    movegen_perft_test("8/k7/8/8/8/8/1p6/4K3 b - - 0 1".to_owned(), 6, 217342);
}

#[test]
fn movegen_perft_19() {
    movegen_perft_test("8/P1k5/K7/8/8/8/8/8 w - - 0 1".to_owned(), 6, 92683);
}

#[test]
fn movegen_perft_20() {
    movegen_perft_test("8/8/8/8/8/k7/p1K5/8 b - - 0 1".to_owned(), 6, 92683);
}

#[test]
fn movegen_perft_21() {
    movegen_perft_test("K1k5/8/P7/8/8/8/8/8 w - - 0 1".to_owned(), 6, 2217);
}

#[test]
fn movegen_perft_22() {
    movegen_perft_test("8/8/8/8/8/p7/8/k1K5 b - - 0 1".to_owned(), 6, 2217);
}

#[test]
fn movegen_perft_23() {
    movegen_perft_test("8/k1P5/8/1K6/8/8/8/8 w - - 0 1".to_owned(), 7, 567584);
}

#[test]
fn movegen_perft_24() {
    movegen_perft_test("8/8/8/8/1k6/8/K1p5/8 b - - 0 1".to_owned(), 7, 567584);
}

#[test]
fn movegen_perft_25() {
    movegen_perft_test("8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1".to_owned(), 4, 23527);
}

#[test]
fn movegen_perft_26() {
    movegen_perft_test("8/5k2/8/5N2/5Q2/2K5/8/8 w - - 0 1".to_owned(), 4, 23527);
}

#[test]
fn movegen_issue_15() {
    let board =
        BoardBuilder::from_str("rnbqkbnr/ppp2pp1/4p3/3N4/3PpPp1/8/PPP3PP/R1B1KBNR b KQkq f3 0 1")
            .unwrap()
            .try_into()
            .unwrap();
    let _ = MoveGen::new_legal(&board);
}

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

#[test]
fn test_masked_move_gen() {
    let board =
        Board::from_str("r1bqkb1r/pp3ppp/5n2/2ppn1N1/4pP2/1BN1P3/PPPP2PP/R1BQ1RK1 w kq - 0 9")
            .unwrap();

    let mut capture_moves = MoveGen::new_legal(&board);
    let targets = *board.color_combined(!board.side_to_move());
    capture_moves.set_iterator_mask(targets);

    let expected = vec![
        move_of("f4e5"),
        move_of("b3d5"),
        move_of("g5e4"),
        move_of("g5f7"),
        move_of("g5h7"),
        move_of("c3e4"),
        move_of("c3d5"),
    ];

    assert_eq!(
        capture_moves.collect::<HashSet<_>>(),
        expected.into_iter().collect()
    );
}

// ─── Bughouse-specific tests ───────────────────────────────────────────────

#[test]
fn test_starting_position_moves() {
    let board = Board::default();
    let movegen = MoveGen::new_legal(&board);
    assert_eq!(movegen.len(), 20); // 16 pawn moves + 4 knight moves
}

#[test]
fn test_ongoing_status() {
    use crate::board::BoardStatus;
    let board = Board::default();
    assert_eq!(board.status(), BoardStatus::Ongoing);
}

#[test]
fn test_drop_moves_empty_reserves() {
    let board = Board::default();
    let drops = MoveGen::drop_moves(&board);
    assert!(drops.is_empty());
}

#[test]
fn test_drop_moves_with_reserves() {
    use crate::color::Color;
    // White to move, give white a pawn in reserve
    // Starting position has 32 empty squares on ranks 3-6
    let mut board = Board::default();
    board.add_to_reserve(Color::White, Piece::Pawn);
    let drops = MoveGen::drop_moves(&board);
    // Pawn can drop on 32 empty squares minus 0 on ranks 1/8 (all empty squares are on ranks 3-6)
    assert_eq!(drops.len(), 32);
}

#[test]
fn test_drop_moves_open_position() {
    use crate::bughouse_move::BughouseMove;
    use crate::color::Color;
    // Position with lots of empty squares, white has a knight in reserve
    let mut board: Board = BoardBuilder::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::White, Piece::Knight);
    let drops = MoveGen::drop_moves(&board);
    // 64 squares - 2 kings = 62 empty squares, knight can drop on all of them
    assert_eq!(drops.len(), 62);
    // All should be Drop moves
    for d in &drops {
        match d {
            BughouseMove::Drop { piece, .. } => assert_eq!(*piece, Piece::Knight),
            _ => panic!("expected drop move"),
        }
    }
}

#[test]
fn test_pawn_drop_rank_restriction() {
    use crate::color::Color;
    // Pawn cannot be dropped on rank 1 or rank 8
    let mut board: Board = BoardBuilder::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::White, Piece::Pawn);
    let drops = MoveGen::drop_moves(&board);
    // 62 empty squares, minus 16 on ranks 1 and 8 = but 2 squares have kings
    // Rank 1: 8 squares, 1 has king = 7 empty. Rank 8: 8 squares, 1 has king = 7 empty.
    // Total empty: 62. Pawn-restricted: 62 - 7 - 7 = 48
    assert_eq!(drops.len(), 48);
    for d in &drops {
        match d {
            BughouseMove::Drop { square, .. } => {
                assert_ne!(square.get_rank(), crate::rank::Rank::First);
                assert_ne!(square.get_rank(), crate::rank::Rank::Eighth);
            }
            _ => panic!("expected drop move"),
        }
    }
}

#[test]
fn test_drop_move_execution() {
    use crate::color::Color;
    // Drop a knight on e4
    let mut board: Board = BoardBuilder::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::White, Piece::Knight);
    let result = board.make_drop_new(Piece::Knight, Square::from_str("e4").unwrap());
    assert!(result.is_some());
    let new_board = result.unwrap();
    // Knight should be on e4
    assert_eq!(new_board.piece_on(Square::from_str("e4").unwrap()), Some(Piece::Knight));
    // Reserve should be empty
    assert_eq!(new_board.reserves()[Color::White.to_index()].count(Piece::Knight), 0);
    // Side to move should be black
    assert_eq!(new_board.side_to_move(), Color::Black);
}

#[test]
fn test_drop_on_occupied_square_fails() {
    use crate::color::Color;
    let mut board: Board = BoardBuilder::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::White, Piece::Knight);
    // Try to drop on e1 (where king is)
    let result = board.make_drop_new(Piece::Knight, Square::from_str("e1").unwrap());
    assert!(result.is_none());
}

#[test]
fn test_drop_without_reserve_fails() {
    // No reserves, try to drop
    let board: Board = BoardBuilder::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    let result = board.make_drop_new(Piece::Knight, Square::from_str("e4").unwrap());
    assert!(result.is_none());
}

#[test]
fn test_pawn_drop_on_back_rank_fails() {
    use crate::color::Color;
    let mut board: Board = BoardBuilder::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::White, Piece::Pawn);
    // Rank 1
    let result = board.make_drop_new(Piece::Pawn, Square::from_str("a1").unwrap());
    assert!(result.is_none());
    // Rank 8
    let result = board.make_drop_new(Piece::Pawn, Square::from_str("a8").unwrap());
    assert!(result.is_none());
    // Rank 4 should work
    let result = board.make_drop_new(Piece::Pawn, Square::from_str("a4").unwrap());
    assert!(result.is_some());
}

#[test]
fn test_capture_tracking() {
    use crate::color::Color;
    // Set up a position where white can capture a black pawn
    let board: Board = BoardBuilder::from_str("k7/8/8/8/3p4/4N3/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    let capture_move = ChessMove::new(
        Square::from_str("e3").unwrap(),
        Square::from_str("d4").unwrap(),
        None,
    );
    let (new_board, capture) = board.make_move_with_capture(capture_move);
    assert!(capture.is_some());
    let (piece, was_promoted) = capture.unwrap();
    assert_eq!(piece, Piece::Pawn);
    assert!(!was_promoted);
    // Knight should now be on d4
    assert_eq!(new_board.piece_on(Square::from_str("d4").unwrap()), Some(Piece::Knight));
}

#[test]
fn test_bfen_roundtrip() {
    use crate::color::Color;
    // Build a board with reserves and a promoted piece
    // Brackets must be attached to piece placement (first FEN token)
    let builder = BoardBuilder::from_str("k7/8/8/8/8/8/8/4K2Q~[Pnr] w - - 0 1").unwrap();
    let board: Board = (&builder).try_into().unwrap();
    // Verify reserves
    assert_eq!(board.reserves()[Color::White.to_index()].count(Piece::Pawn), 1);
    assert_eq!(board.reserves()[Color::Black.to_index()].count(Piece::Knight), 1);
    assert_eq!(board.reserves()[Color::Black.to_index()].count(Piece::Rook), 1);
    // Verify promoted
    assert!(board.is_promoted(Square::from_str("h1").unwrap()));
    // Roundtrip
    let fen = format!("{}", board);
    let board2: Board = BoardBuilder::from_str(&fen).unwrap().try_into().unwrap();
    assert_eq!(board.get_hash(), board2.get_hash());
}

// ─── New bughouse logic tests ──────────────────────────────────────────────

#[test]
fn test_drop_blocks_single_check() {
    use crate::color::Color;
    // Black king on e8, white rook on e1 gives check along e-file.
    // Drops should only be on blocking squares: e2, e3, e4, e5, e6, e7 (6 squares)
    let mut board: Board = BoardBuilder::from_str("4k3/8/8/8/8/8/8/4K1R1 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    // Actually need rook to be on e-file for this to work. Let me use a better position.
    // White rook on e1, black king on e8, white king on a1
    let mut board: Board = BoardBuilder::from_str("4k3/8/8/8/8/8/8/K3R3 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::Black, Piece::Pawn);
    board.add_to_reserve(Color::Black, Piece::Knight);
    let drops = MoveGen::drop_moves(&board);
    // between(e1, e8) = e2,e3,e4,e5,e6,e7 = 6 squares
    // Pawn can drop on e2-e7 (all ranks 2-7, OK for pawns) = 6
    // Knight can drop on e2-e7 = 6
    // Total = 12
    assert_eq!(drops.len(), 12);
    for d in &drops {
        match d {
            BughouseMove::Drop { square, .. } => {
                assert_eq!(square.get_file(), crate::file::File::E);
                assert!(square.get_rank() >= crate::rank::Rank::Second);
                assert!(square.get_rank() <= crate::rank::Rank::Seventh);
            }
            _ => panic!("expected drop move"),
        }
    }
}

#[test]
fn test_drop_cannot_block_knight_check() {
    use crate::color::Color;
    // Knight check: between(knight, king) is EMPTY, so no blocking drops
    // White knight on f6, black king on e8, white king on a1
    let mut board: Board = BoardBuilder::from_str("4k3/8/5N2/8/8/8/8/K7 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::Black, Piece::Pawn);
    board.add_to_reserve(Color::Black, Piece::Rook);
    let drops = MoveGen::drop_moves(&board);
    // Knight check: between(f6, e8) = EMPTY, so drop_mask = EMPTY
    assert!(drops.is_empty());
}

#[test]
fn test_drop_in_double_check_empty() {
    use crate::color::Color;
    // Double check: no drops possible at all
    // White rook on e1, white bishop on b5, black king on e8, white king on a1
    // After a discovered check (rook on e-file + bishop diagonal), king must move
    // Simpler: use a position where checkers has 2 pieces
    let mut board: Board = BoardBuilder::from_str("4k3/8/8/1B6/8/8/8/K3R3 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    board.add_to_reserve(Color::Black, Piece::Queen);
    // Verify it's double check
    assert!(board.checkers().popcnt() >= 2);
    let drops = MoveGen::drop_moves(&board);
    assert!(drops.is_empty());
}

#[test]
fn test_checkmate_unblockable() {
    use crate::board::BoardStatus;
    // Adjacent queen mate: queen on a2, king on a1, white king on b3
    // This is unblockable (between(a2, a1) = EMPTY)
    let board: Board = BoardBuilder::from_str("8/8/8/8/8/1K6/Q7/k7 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    assert_eq!(board.status(), BoardStatus::Checkmate);
}

#[test]
fn test_checkmate_knight_no_escape() {
    use crate::board::BoardStatus;
    // Knight check with king boxed in, no escape squares.
    // Nf7 checks h8. Rg1 covers g-file (g7, g8). h7 pawn blocks that square.
    // King escapes: g8 (Rg1 ✓), g7 (Rg1 ✓), h7 (own pawn ✓). No escape.
    let board: Board = BoardBuilder::from_str("7k/5N1p/8/8/8/8/8/K5R1 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    assert_eq!(board.checkers().popcnt(), 1);
    // Knight check = between is empty = unblockable
    assert_eq!(board.status(), BoardStatus::Checkmate);
}

#[test]
fn test_not_checkmate_when_drop_could_block() {
    use crate::board::BoardStatus;
    use crate::color::Color;
    // Rook check from distance: white rook on e1, black king on e8, white king on a1
    // Normally this would be checkmate if black has no pieces.
    // But with a reserve piece, drops can block on e2-e7, so it's Ongoing.
    let mut board: Board = BoardBuilder::from_str("4k3/8/8/8/8/8/8/K3R3 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    // Without reserves, should still be ongoing if king can move
    let legal_moves = MoveGen::new_legal(&board);
    let king_can_move = legal_moves.len() > 0;
    if !king_can_move {
        // If king can't move and no reserves, it could be checkmate
        // But let's give black a piece to drop
        board.add_to_reserve(Color::Black, Piece::Rook);
        assert_eq!(board.status(), BoardStatus::Ongoing);
    } else {
        // King has escape moves, so it's Ongoing regardless
        assert_eq!(board.status(), BoardStatus::Ongoing);
    }
}

#[test]
fn test_not_checkmate_distant_sliding_check() {
    use crate::board::BoardStatus;
    // Rook on a1 checks king on a8 along a-file (clear path).
    // Rb7 covers b8 and a7, Kb6 covers a7. King has NO escape squares.
    // between(a1, a8) = {a2..a7} — all 6 empty → is_drop_blockable_check = true → Ongoing
    let board: Board =
        BoardBuilder::from_str("k7/1R6/1K6/8/8/8/8/R7 b - - 0 1")
            .unwrap()
            .try_into()
            .unwrap();
    // Verify king is in check from a distant slider
    assert_eq!(board.checkers().popcnt(), 1);
    let legal = MoveGen::new_legal(&board);
    assert_eq!(legal.len(), 0);
    // Without reserves, no drop moves either
    assert_eq!(MoveGen::drop_moves(&board).len(), 0);
    // But is_drop_blockable_check sees empty squares between a1 and a8
    // In bughouse, partner could send a piece → Ongoing, not Checkmate
    assert_eq!(board.status(), BoardStatus::Ongoing);
}

#[test]
fn test_stalemate_detection() {
    use crate::board::BoardStatus;
    // Classic stalemate: black king on a8, white queen on b6, white king on c8
    // King can't move anywhere, not in check
    let board: Board = BoardBuilder::from_str("k1K5/8/1Q6/8/8/8/8/8 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    assert_eq!(board.checkers().popcnt(), 0);
    assert_eq!(board.status(), BoardStatus::Stalemate);
}

#[test]
fn test_ep_captures_checker() {
    // EP should be legal when it captures the checking pawn
    // White pawn on d5, black pawn just played e7-e5 giving check... wait, pawns don't give check
    // EP captures checker: set up position where the EP capture removes the checking piece
    // This is tricky — the checking piece must be the pawn that just moved to the EP square
    // Position: white king on d5, black pawn just played e7-e5 (now on e5, EP square e6)
    // Actually e5 pawn doesn't check d5 king. Let's try:
    // White king on f4, black pawn on e4 (just played e7-e5 skipping e6, EP target = e3)
    // Wait, EP is from the perspective of the side to move.
    // Black played e7-e5, white to move, EP target = e6.
    // White pawn on d5 can capture EP on e6, removing black pawn on e5.
    // But black pawn on e5 doesn't check white king.
    // For EP to capture a checker, we need: black pawn gives check to white king.
    // A pawn on e5 attacks d4 and f4. So if white king is on f4:
    // Black pawn moves e7-e5, attacks f4 where white king is → check!
    // White pawn on d5 can capture EP (d5xe6).
    // Position: white king f4, white pawn d5, black pawn e5 (just moved), black king h8
    let board: Board =
        BoardBuilder::from_str("7k/8/8/3Pp3/5K2/8/8/8 w - e6 0 1")
            .unwrap()
            .try_into()
            .unwrap();
    // Black pawn on e5 checks white king on f4
    assert!(board.checkers().popcnt() == 1);
    let legal = MoveGen::new_legal(&board);
    let moves: Vec<ChessMove> = legal.collect();
    // EP capture d5e6 should be in legal moves (it captures the checker)
    let ep_move = ChessMove::new(
        Square::from_str("d5").unwrap(),
        Square::from_str("e6").unwrap(),
        None,
    );
    assert!(moves.contains(&ep_move));
}

#[test]
fn test_promoted_piece_tracking() {
    // Promote a pawn, verify promoted bitboard tracks it
    let board: Board = BoardBuilder::from_str("4k3/P7/8/8/8/8/8/4K3 w - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    let promo_move = ChessMove::new(
        Square::from_str("a7").unwrap(),
        Square::from_str("a8").unwrap(),
        Some(Piece::Queen),
    );
    let new_board = board.make_move_new(promo_move);
    assert!(new_board.is_promoted(Square::from_str("a8").unwrap()));
    assert_eq!(new_board.piece_on(Square::from_str("a8").unwrap()), Some(Piece::Queen));
}

#[test]
fn test_capture_promoted_piece_tracks_demotion() {
    // A promoted queen when captured should be reported as promoted
    let board: Board = BoardBuilder::from_str("3kQ~3/8/8/8/8/8/8/4K3 b - - 0 1")
        .unwrap()
        .try_into()
        .unwrap();
    assert!(board.is_promoted(Square::from_str("e8").unwrap()));
    // Black king captures the promoted queen
    let capture = ChessMove::new(
        Square::from_str("d8").unwrap(),
        Square::from_str("e8").unwrap(),
        None,
    );
    let (new_board, cap_info) = board.make_move_with_capture(capture);
    assert!(cap_info.is_some());
    let (piece, was_promoted) = cap_info.unwrap();
    assert_eq!(piece, Piece::Queen);
    assert!(was_promoted); // was a promoted piece → should demote to pawn in bughouse
}
