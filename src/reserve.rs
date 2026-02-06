use crate::piece::Piece;

/// Piece reserves for bughouse chess.
///
/// Tracks the count of each non-king piece type available for dropping.
/// In bughouse, when you capture an opponent's piece, it goes to your
/// partner's reserve. Your partner can then "drop" it onto any empty square
/// on their turn (pawns cannot be dropped on rank 1 or 8).
///
/// Kings are never captured in the traditional sense (king capture ends the game),
/// so they are never added to reserves.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Default, Hash)]
pub struct Reserve {
    /// Counts indexed by piece type: Pawn=0, Knight=1, Bishop=2, Rook=3, Queen=4
    counts: [u8; 5],
}

/// The piece types that can appear in a reserve (everything except King).
const RESERVE_PIECES: [Piece; 5] = [
    Piece::Pawn,
    Piece::Knight,
    Piece::Bishop,
    Piece::Rook,
    Piece::Queen,
];

impl Reserve {
    /// Create an empty reserve.
    #[inline]
    pub fn new() -> Reserve {
        Reserve { counts: [0; 5] }
    }

    /// Convert a piece to its reserve index.
    /// Panics if the piece is a King.
    #[inline]
    fn piece_index(piece: Piece) -> usize {
        match piece {
            Piece::Pawn => 0,
            Piece::Knight => 1,
            Piece::Bishop => 2,
            Piece::Rook => 3,
            Piece::Queen => 4,
            Piece::King => panic!("Kings cannot be placed in reserves"),
        }
    }

    /// Add a piece to the reserve. Panics if the piece is a King.
    #[inline]
    pub fn add(&mut self, piece: Piece) {
        self.counts[Self::piece_index(piece)] += 1;
    }

    /// Remove a piece from the reserve. Returns true if the piece was available
    /// and removed, false if none of that piece type were in reserve.
    #[inline]
    pub fn remove(&mut self, piece: Piece) -> bool {
        let idx = Self::piece_index(piece);
        if self.counts[idx] > 0 {
            self.counts[idx] -= 1;
            true
        } else {
            false
        }
    }

    /// How many of this piece type are in the reserve?
    #[inline]
    pub fn count(&self, piece: Piece) -> u8 {
        self.counts[Self::piece_index(piece)]
    }

    /// Is the reserve completely empty?
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.counts.iter().all(|&c| c == 0)
    }

    /// Total number of pieces in reserve.
    #[inline]
    pub fn total(&self) -> u8 {
        self.counts.iter().sum()
    }

    /// Iterate over (Piece, count) pairs for piece types with count > 0.
    pub fn iter(&self) -> impl Iterator<Item = (Piece, u8)> + '_ {
        RESERVE_PIECES
            .iter()
            .zip(self.counts.iter())
            .filter(|(_, &count)| count > 0)
            .map(|(&piece, &count)| (piece, count))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_empty_reserve() {
        let r = Reserve::new();
        assert!(r.is_empty());
        assert_eq!(r.total(), 0);
        assert_eq!(r.count(Piece::Pawn), 0);
        assert_eq!(r.count(Piece::Queen), 0);
    }

    #[test]
    fn test_add_and_count() {
        let mut r = Reserve::new();
        r.add(Piece::Pawn);
        r.add(Piece::Pawn);
        r.add(Piece::Knight);
        assert_eq!(r.count(Piece::Pawn), 2);
        assert_eq!(r.count(Piece::Knight), 1);
        assert_eq!(r.total(), 3);
        assert!(!r.is_empty());
    }

    #[test]
    fn test_remove() {
        let mut r = Reserve::new();
        r.add(Piece::Rook);
        assert!(r.remove(Piece::Rook));
        assert!(!r.remove(Piece::Rook));
        assert!(r.is_empty());
    }

    #[test]
    fn test_iter() {
        let mut r = Reserve::new();
        r.add(Piece::Queen);
        r.add(Piece::Pawn);
        r.add(Piece::Pawn);
        let items: Vec<(Piece, u8)> = r.iter().collect();
        assert_eq!(items.len(), 2);
        assert!(items.contains(&(Piece::Pawn, 2)));
        assert!(items.contains(&(Piece::Queen, 1)));
    }

    #[test]
    #[should_panic(expected = "Kings cannot be placed in reserves")]
    fn test_king_panics() {
        let mut r = Reserve::new();
        r.add(Piece::King);
    }
}
