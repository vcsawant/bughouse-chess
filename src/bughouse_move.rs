use crate::chess_move::ChessMove;
use crate::error::Error;
use crate::piece::Piece;
use crate::square::Square;

use std::fmt;
use std::str::FromStr;

/// A move in bughouse chess — either a regular board move or a piece drop from reserves.
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum BughouseMove {
    /// A regular chess move (piece moves from one square to another, possibly with promotion).
    Regular(ChessMove),
    /// A drop move: place a piece from reserves onto an empty square.
    Drop {
        piece: Piece,
        square: Square,
    },
}

impl BughouseMove {
    /// Create a regular move.
    #[inline]
    pub fn regular(chess_move: ChessMove) -> BughouseMove {
        BughouseMove::Regular(chess_move)
    }

    /// Create a drop move.
    #[inline]
    pub fn drop_piece(piece: Piece, square: Square) -> BughouseMove {
        BughouseMove::Drop { piece, square }
    }

    /// Is this a drop move?
    #[inline]
    pub fn is_drop(&self) -> bool {
        matches!(self, BughouseMove::Drop { .. })
    }
}

impl fmt::Display for BughouseMove {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BughouseMove::Regular(m) => write!(f, "{}", m),
            BughouseMove::Drop { piece, square } => {
                // BUP drop notation: p@e4 (lowercase piece letter, @, square)
                let piece_char = match piece {
                    Piece::Pawn => 'p',
                    Piece::Knight => 'n',
                    Piece::Bishop => 'b',
                    Piece::Rook => 'r',
                    Piece::Queen => 'q',
                    Piece::King => 'k',
                };
                write!(f, "{}@{}", piece_char, square)
            }
        }
    }
}

impl FromStr for BughouseMove {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Check for drop notation: X@sq (e.g., "P@e4", "n@f3")
        if s.len() == 4 && s.as_bytes()[1] == b'@' {
            let piece = match s.as_bytes()[0] {
                b'P' | b'p' => Piece::Pawn,
                b'N' | b'n' => Piece::Knight,
                b'B' | b'b' => Piece::Bishop,
                b'R' | b'r' => Piece::Rook,
                b'Q' | b'q' => Piece::Queen,
                _ => return Err(Error::InvalidUciMove),
            };
            let square = Square::from_str(&s[2..4])?;
            Ok(BughouseMove::Drop { piece, square })
        } else {
            // Try parsing as a regular UCI move
            let chess_move = ChessMove::from_str(s)?;
            Ok(BughouseMove::Regular(chess_move))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_drop_display() {
        let m = BughouseMove::drop_piece(Piece::Pawn, Square::from_str("e4").unwrap());
        assert_eq!(format!("{}", m), "p@e4");
    }

    #[test]
    fn test_drop_parse() {
        let m: BughouseMove = "P@e4".parse().unwrap();
        assert_eq!(
            m,
            BughouseMove::Drop {
                piece: Piece::Pawn,
                square: Square::from_str("e4").unwrap()
            }
        );

        let m2: BughouseMove = "n@f3".parse().unwrap();
        assert_eq!(
            m2,
            BughouseMove::Drop {
                piece: Piece::Knight,
                square: Square::from_str("f3").unwrap()
            }
        );
    }

    #[test]
    fn test_regular_move_parse() {
        let m: BughouseMove = "e2e4".parse().unwrap();
        assert!(matches!(m, BughouseMove::Regular(_)));
    }
}
