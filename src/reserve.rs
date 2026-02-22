use crate::piece::Piece;

const NUM_PIECES: usize = 5; // Pawn, Knight, Bishop, Rook, Queen (no King drops)
const PIECES: [Piece; NUM_PIECES] = [
    Piece::Pawn,
    Piece::Knight,
    Piece::Bishop,
    Piece::Rook,
    Piece::Queen,
];

/// Tracks pieces available for dropping in bughouse.
/// Kings are never held in reserve.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Reserve {
    counts: [u8; NUM_PIECES],
}

impl Reserve {
    /// Create an empty reserve.
    pub fn new() -> Self {
        Reserve {
            counts: [0; NUM_PIECES],
        }
    }

    fn index(piece: Piece) -> usize {
        match piece {
            Piece::Pawn => 0,
            Piece::Knight => 1,
            Piece::Bishop => 2,
            Piece::Rook => 3,
            Piece::Queen => 4,
            Piece::King => panic!("King cannot be held in reserve"),
        }
    }

    /// Add one of the given piece to the reserve.
    pub fn add(&mut self, piece: Piece) {
        self.counts[Self::index(piece)] += 1;
    }

    /// Remove one of the given piece from the reserve. Returns false if none available.
    pub fn remove(&mut self, piece: Piece) -> bool {
        let idx = Self::index(piece);
        if self.counts[idx] > 0 {
            self.counts[idx] -= 1;
            true
        } else {
            false
        }
    }

    /// How many of a given piece are in the reserve?
    pub fn count(&self, piece: Piece) -> u8 {
        self.counts[Self::index(piece)]
    }

    /// Is the reserve completely empty?
    pub fn is_empty(&self) -> bool {
        self.counts.iter().all(|&c| c == 0)
    }

    /// Total number of pieces in reserve.
    pub fn total(&self) -> u8 {
        self.counts.iter().sum()
    }

    /// Iterate over (piece, count) pairs for non-zero counts.
    pub fn iter(&self) -> impl Iterator<Item = (Piece, u8)> + '_ {
        PIECES
            .iter()
            .zip(self.counts.iter())
            .filter(|(_, &c)| c > 0)
            .map(|(&p, &c)| (p, c))
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
    }

    #[test]
    fn test_add_and_count() {
        let mut r = Reserve::new();
        r.add(Piece::Pawn);
        r.add(Piece::Pawn);
        r.add(Piece::Queen);
        assert_eq!(r.count(Piece::Pawn), 2);
        assert_eq!(r.count(Piece::Queen), 1);
        assert_eq!(r.count(Piece::Knight), 0);
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
        r.add(Piece::Pawn);
        r.add(Piece::Bishop);
        r.add(Piece::Bishop);
        let items: Vec<_> = r.iter().collect();
        assert_eq!(items, vec![(Piece::Pawn, 1), (Piece::Bishop, 2)]);
    }

    #[test]
    #[should_panic(expected = "King cannot be held in reserve")]
    fn test_king_panics() {
        let mut r = Reserve::new();
        r.add(Piece::King);
    }
}
