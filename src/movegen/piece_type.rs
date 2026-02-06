use crate::bitboard::{BitBoard, EMPTY};
use crate::board::Board;
use crate::color::Color;
use crate::movegen::{MoveList, SquareAndBitBoard};
use crate::piece::Piece;
use crate::square::Square;

use crate::magic::{
    get_adjacent_files, get_bishop_moves, get_bishop_rays, get_king_moves, get_knight_moves,
    get_pawn_attacks, get_pawn_moves, get_rank, get_rook_moves, get_rook_rays,
};

pub trait PieceType {
    fn is(piece: Piece) -> bool;
    fn into_piece() -> Piece;
    #[inline(always)]
    fn pseudo_legals(src: Square, color: Color, combined: BitBoard, mask: BitBoard) -> BitBoard;
    /// Generate all pseudo-legal moves for this piece type.
    ///
    /// In bughouse, there are no pin or check restrictions on piece movement
    /// (king can move into check, pieces can move while pinned), so we generate
    /// all pseudo-legal moves without filtering.
    #[inline(always)]
    fn legals<T>(movelist: &mut MoveList, board: &Board, mask: BitBoard)
    where
        T: CheckType,
    {
        let combined = board.combined();
        let color = board.side_to_move();
        let my_pieces = board.color_combined(color);

        let pieces = board.pieces(Self::into_piece()) & my_pieces;

        // In bughouse: no pin/check filtering. All pieces can move freely.
        for src in pieces {
            let moves = Self::pseudo_legals(src, color, *combined, mask);
            if moves != EMPTY {
                unsafe {
                    movelist.push_unchecked(SquareAndBitBoard::new(src, moves, false));
                }
            }
        }
    }
}

pub struct PawnType;
pub struct BishopType;
pub struct KnightType;
pub struct RookType;
pub struct QueenType;
pub struct KingType;

pub trait CheckType {
    const IN_CHECK: bool;
}

pub struct InCheckType;
pub struct NotInCheckType;

impl CheckType for InCheckType {
    const IN_CHECK: bool = true;
}

impl CheckType for NotInCheckType {
    const IN_CHECK: bool = false;
}

impl PawnType {
    /// Is a particular en-passant capture legal?
    pub fn legal_ep_move(board: &Board, source: Square, dest: Square) -> bool {
        let combined = board.combined()
            ^ BitBoard::from_square(board.en_passant().unwrap())
            ^ BitBoard::from_square(source)
            ^ BitBoard::from_square(dest);

        let ksq =
            (board.pieces(Piece::King) & board.color_combined(board.side_to_move())).to_square();

        let rooks = (board.pieces(Piece::Rook) | board.pieces(Piece::Queen))
            & board.color_combined(!board.side_to_move());

        if (get_rook_rays(ksq) & rooks) != EMPTY {
            if (get_rook_moves(ksq, combined) & rooks) != EMPTY {
                return false;
            }
        }

        let bishops = (board.pieces(Piece::Bishop) | board.pieces(Piece::Queen))
            & board.color_combined(!board.side_to_move());

        if (get_bishop_rays(ksq) & bishops) != EMPTY {
            if (get_bishop_moves(ksq, combined) & bishops) != EMPTY {
                return false;
            }
        }

        return true;
    }
}

impl PieceType for PawnType {
    fn is(piece: Piece) -> bool {
        piece == Piece::Pawn
    }

    fn into_piece() -> Piece {
        Piece::Pawn
    }

    #[inline(always)]
    fn pseudo_legals(src: Square, color: Color, combined: BitBoard, mask: BitBoard) -> BitBoard {
        get_pawn_moves(src, color, combined) & mask
    }

    #[inline(always)]
    fn legals<T>(movelist: &mut MoveList, board: &Board, mask: BitBoard)
    where
        T: CheckType,
    {
        let combined = board.combined();
        let color = board.side_to_move();
        let my_pieces = board.color_combined(color);

        let pieces = board.pieces(Self::into_piece()) & my_pieces;

        // In bughouse: no pin/check filtering for pawns.
        for src in pieces {
            let moves = Self::pseudo_legals(src, color, *combined, mask);
            if moves != EMPTY {
                unsafe {
                    movelist.push_unchecked(SquareAndBitBoard::new(
                        src,
                        moves,
                        src.get_rank() == color.to_seventh_rank(),
                    ));
                }
            }
        }

        // En passant: always legal in bughouse (no pin check needed).
        if board.en_passant().is_some() {
            let ep_sq = board.en_passant().unwrap();
            let rank = get_rank(ep_sq.get_rank());
            let files = get_adjacent_files(ep_sq.get_file());
            for src in rank & files & pieces {
                let dest = ep_sq.uforward(color);
                unsafe {
                    movelist.push_unchecked(SquareAndBitBoard::new(
                        src,
                        BitBoard::from_square(dest),
                        false,
                    ));
                }
            }
        }
    }
}

impl PieceType for BishopType {
    fn is(piece: Piece) -> bool {
        piece == Piece::Bishop
    }

    fn into_piece() -> Piece {
        Piece::Bishop
    }

    #[inline(always)]
    fn pseudo_legals(src: Square, _color: Color, combined: BitBoard, mask: BitBoard) -> BitBoard {
        get_bishop_moves(src, combined) & mask
    }
}

impl PieceType for KnightType {
    fn is(piece: Piece) -> bool {
        piece == Piece::Knight
    }

    fn into_piece() -> Piece {
        Piece::Knight
    }

    #[inline(always)]
    fn pseudo_legals(src: Square, _color: Color, _combined: BitBoard, mask: BitBoard) -> BitBoard {
        get_knight_moves(src) & mask
    }

    // Knights use the default legals implementation (no pin/check filtering in bughouse).
}

impl PieceType for RookType {
    fn is(piece: Piece) -> bool {
        piece == Piece::Rook
    }

    fn into_piece() -> Piece {
        Piece::Rook
    }

    #[inline(always)]
    fn pseudo_legals(src: Square, _color: Color, combined: BitBoard, mask: BitBoard) -> BitBoard {
        get_rook_moves(src, combined) & mask
    }
}

impl PieceType for QueenType {
    fn is(piece: Piece) -> bool {
        piece == Piece::Queen
    }

    fn into_piece() -> Piece {
        Piece::Queen
    }

    #[inline(always)]
    fn pseudo_legals(src: Square, _color: Color, combined: BitBoard, mask: BitBoard) -> BitBoard {
        (get_rook_moves(src, combined) ^ get_bishop_moves(src, combined)) & mask
    }
}

impl KingType {
    /// Is a particular king move legal?
    #[inline(always)]
    pub fn legal_king_move(board: &Board, dest: Square) -> bool {
        let combined = board.combined()
            ^ (board.pieces(Piece::King) & board.color_combined(board.side_to_move()))
            | BitBoard::from_square(dest);

        let mut attackers = EMPTY;

        let rooks = (board.pieces(Piece::Rook) | board.pieces(Piece::Queen))
            & board.color_combined(!board.side_to_move());

        attackers |= get_rook_moves(dest, combined) & rooks;

        let bishops = (board.pieces(Piece::Bishop) | board.pieces(Piece::Queen))
            & board.color_combined(!board.side_to_move());

        attackers |= get_bishop_moves(dest, combined) & bishops;

        let knight_rays = get_knight_moves(dest);
        attackers |=
            knight_rays & board.pieces(Piece::Knight) & board.color_combined(!board.side_to_move());

        let king_rays = get_king_moves(dest);
        attackers |=
            king_rays & board.pieces(Piece::King) & board.color_combined(!board.side_to_move());

        attackers |= get_pawn_attacks(
            dest,
            board.side_to_move(),
            board.pieces(Piece::Pawn) & board.color_combined(!board.side_to_move()),
        );

        return attackers == EMPTY;
    }
}

impl PieceType for KingType {
    fn is(piece: Piece) -> bool {
        piece == Piece::King
    }

    fn into_piece() -> Piece {
        Piece::King
    }

    #[inline(always)]
    fn pseudo_legals(src: Square, _color: Color, _combined: BitBoard, mask: BitBoard) -> BitBoard {
        get_king_moves(src) & mask
    }

    /// Generate all king moves for bughouse.
    ///
    /// In bughouse, the king can move to ANY square not occupied by own pieces,
    /// including squares attacked by the opponent. Castling is allowed regardless
    /// of check status, and the king can castle through attacked squares.
    /// Only requirement for castling: squares between king and rook must be empty.
    #[inline(always)]
    fn legals<T>(movelist: &mut MoveList, board: &Board, mask: BitBoard)
    where
        T: CheckType,
    {
        let combined = board.combined();
        let color = board.side_to_move();
        let ksq = board.king_square(color);

        // All pseudo-legal king moves (no attack filtering in bughouse)
        let mut moves = Self::pseudo_legals(ksq, color, *combined, mask);

        // Castling: allowed even when in check, and through attacked squares.
        // Only check: castle rights exist and squares between king and rook are empty.
        if board.my_castle_rights().has_kingside()
            && (combined & board.my_castle_rights().kingside_squares(color)) == EMPTY
        {
            let right = ksq.uright().uright();
            moves ^= BitBoard::from_square(right);
        }

        if board.my_castle_rights().has_queenside()
            && (combined & board.my_castle_rights().queenside_squares(color)) == EMPTY
        {
            let left = ksq.uleft().uleft();
            moves ^= BitBoard::from_square(left);
        }

        if moves != EMPTY {
            unsafe {
                movelist.push_unchecked(SquareAndBitBoard::new(ksq, moves, false));
            }
        }
    }
}
