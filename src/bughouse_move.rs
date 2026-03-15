use crate::chess_move::ChessMove;
use crate::piece::Piece;
use crate::square::Square;
use std::fmt;
use std::str::FromStr;

/// Represents either a regular chess move or a bughouse drop.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BughouseMove {
    /// A standard chess move (from-square to-square, optional promotion).
    Regular(ChessMove),
    /// A piece drop from reserve onto a square.
    Drop { piece: Piece, square: Square },
}

impl BughouseMove {
    /// Compress to u16 for transposition table storage.
    ///
    /// Regular moves (bit 15 = 0): `[0][promo:3][dest:6][source:6]`
    /// Drop moves (bit 15 = 1):    `[1][unused:6][piece:3][dest:6]`
    ///
    /// `0u16` represents "no move" (a1→a1, not a legal chess move).
    #[inline]
    pub fn compress(&self) -> u16 {
        match self {
            BughouseMove::Regular(cm) => {
                let src = cm.get_source().to_int() as u16;
                let dst = cm.get_dest().to_int() as u16;
                let promo = match cm.get_promotion() {
                    None => 0u16,
                    Some(Piece::Queen) => 1,
                    Some(Piece::Knight) => 2,
                    Some(Piece::Rook) => 3,
                    Some(Piece::Bishop) => 4,
                    Some(_) => 0, // shouldn't happen
                };
                src | (dst << 6) | (promo << 12)
            }
            BughouseMove::Drop { piece, square } => {
                let dst = square.to_int() as u16;
                let pc = piece.to_index() as u16; // 0-4 for droppable pieces
                (1 << 15) | dst | (pc << 6)
            }
        }
    }

    /// Decompress from u16. Returns `None` for invalid encoding or `0u16` (no move).
    #[inline]
    pub fn decompress(encoded: u16) -> Option<BughouseMove> {
        if encoded == 0 {
            return None;
        }
        if encoded & (1 << 15) != 0 {
            // Drop move
            let dst = (encoded & 0x3F) as u8;
            let pc_idx = ((encoded >> 6) & 0x07) as usize;
            let piece = Piece::from_index(pc_idx)?;
            if piece == Piece::King {
                return None; // can't drop a king
            }
            Some(BughouseMove::Drop {
                piece,
                square: Square::new(dst),
            })
        } else {
            // Regular move
            let src = (encoded & 0x3F) as u8;
            let dst = ((encoded >> 6) & 0x3F) as u8;
            let promo_bits = (encoded >> 12) & 0x07;
            let promotion = match promo_bits {
                0 => None,
                1 => Some(Piece::Queen),
                2 => Some(Piece::Knight),
                3 => Some(Piece::Rook),
                4 => Some(Piece::Bishop),
                _ => return None,
            };
            Some(BughouseMove::Regular(ChessMove::new(
                Square::new(src),
                Square::new(dst),
                promotion,
            )))
        }
    }
}

impl fmt::Display for BughouseMove {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BughouseMove::Regular(m) => write!(f, "{}", m),
            BughouseMove::Drop { piece, square } => {
                // Lowercase piece letter for drop notation: p@e4
                let c = match piece {
                    Piece::Pawn => 'p',
                    Piece::Knight => 'n',
                    Piece::Bishop => 'b',
                    Piece::Rook => 'r',
                    Piece::Queen => 'q',
                    Piece::King => 'k',
                };
                write!(f, "{}@{}", c, square)
            }
        }
    }
}

impl FromStr for BughouseMove {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Drop notation: P@e4 or p@e4
        if s.len() >= 4 && s.as_bytes()[1] == b'@' {
            let piece_char = s.as_bytes()[0];
            let piece = match piece_char {
                b'P' | b'p' => Piece::Pawn,
                b'N' | b'n' => Piece::Knight,
                b'B' | b'b' => Piece::Bishop,
                b'R' | b'r' => Piece::Rook,
                b'Q' | b'q' => Piece::Queen,
                b'K' | b'k' => Piece::King,
                _ => return Err(format!("Invalid drop piece: {}", piece_char as char)),
            };
            let square = Square::from_str(&s[2..])
                .map_err(|_| format!("Invalid drop square: {}", &s[2..]))?;
            Ok(BughouseMove::Drop { piece, square })
        } else {
            // Regular UCI move: e2e4, e7e8q
            let chess_move = ChessMove::from_str(s)
                .map_err(|_| format!("Invalid chess move: {}", s))?;
            Ok(BughouseMove::Regular(chess_move))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_drop_display() {
        let m = BughouseMove::Drop {
            piece: Piece::Pawn,
            square: Square::from_str("e4").unwrap(),
        };
        assert_eq!(format!("{}", m), "p@e4");
    }

    #[test]
    fn test_drop_parse_lowercase() {
        let m: BughouseMove = "p@e4".parse().unwrap();
        assert_eq!(
            m,
            BughouseMove::Drop {
                piece: Piece::Pawn,
                square: Square::from_str("e4").unwrap(),
            }
        );
    }

    #[test]
    fn test_drop_parse_uppercase() {
        let m: BughouseMove = "Q@d5".parse().unwrap();
        assert_eq!(
            m,
            BughouseMove::Drop {
                piece: Piece::Queen,
                square: Square::from_str("d5").unwrap(),
            }
        );
    }

    #[test]
    fn test_regular_roundtrip() {
        let m: BughouseMove = "e2e4".parse().unwrap();
        if let BughouseMove::Regular(cm) = m {
            assert_eq!(format!("{}", cm), "e2e4");
        } else {
            panic!("Expected Regular move");
        }
    }

    #[test]
    fn test_drop_roundtrip() {
        let original = "n@f3";
        let m: BughouseMove = original.parse().unwrap();
        assert_eq!(format!("{}", m), original);
    }

    #[test]
    fn test_compress_regular_move() {
        let m: BughouseMove = "e2e4".parse().unwrap();
        let compressed = m.compress();
        let decompressed = BughouseMove::decompress(compressed).unwrap();
        assert_eq!(m, decompressed);
    }

    #[test]
    fn test_compress_promotion() {
        let m: BughouseMove = "e7e8q".parse().unwrap();
        let compressed = m.compress();
        let decompressed = BughouseMove::decompress(compressed).unwrap();
        assert_eq!(m, decompressed);
    }

    #[test]
    fn test_compress_castling() {
        let m: BughouseMove = "e1g1".parse().unwrap();
        let compressed = m.compress();
        let decompressed = BughouseMove::decompress(compressed).unwrap();
        assert_eq!(m, decompressed);
    }

    #[test]
    fn test_compress_drop() {
        let m: BughouseMove = "n@f3".parse().unwrap();
        let compressed = m.compress();
        let decompressed = BughouseMove::decompress(compressed).unwrap();
        assert_eq!(m, decompressed);
    }

    #[test]
    fn test_compress_queen_drop() {
        let m: BughouseMove = "q@d5".parse().unwrap();
        let compressed = m.compress();
        let decompressed = BughouseMove::decompress(compressed).unwrap();
        assert_eq!(m, decompressed);
    }

    #[test]
    fn test_compress_no_move() {
        assert!(BughouseMove::decompress(0).is_none());
    }

    #[test]
    fn test_compress_all_promotions() {
        for promo in ["q", "n", "r", "b"] {
            let s = format!("e7e8{}", promo);
            let m: BughouseMove = s.parse().unwrap();
            let decompressed = BughouseMove::decompress(m.compress()).unwrap();
            assert_eq!(m, decompressed, "failed for promotion {}", promo);
        }
    }
}
