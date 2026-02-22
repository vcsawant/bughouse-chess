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
}
