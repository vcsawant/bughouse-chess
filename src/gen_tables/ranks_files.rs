use std::fs::File;
use std::io::Write;

use crate::bitboard::{BitBoard, EMPTY};
use crate::file::File as ChessFile;
use crate::rank::Rank;
use crate::square::ALL_SQUARES;

// Given a rank, what squares are on that rank?
static mut RANKS: [BitBoard; 8] = [EMPTY; 8];

// Given a file, what squares are on that file?
static mut FILES: [BitBoard; 8] = [EMPTY; 8];

// Given a file, what squares are adjacent to that file?  Useful for detecting passed pawns.
static mut ADJACENT_FILES: [BitBoard; 8] = [EMPTY; 8];

// What are the EDGES of the board?
static mut EDGES: BitBoard = EMPTY;

// Given a color and square, what squares are on the same + adjacent files ahead?
// Indexed by [color][square]. Used for passed pawn detection in O(1).
// Color: 0 = White (ranks ahead = higher), 1 = Black (ranks ahead = lower).
static mut FORWARD_FILE_MASK: [[BitBoard; 64]; 2] = [[EMPTY; 64]; 2];

// Generate the EDGES, RANKS, FILES, ADJACENT_FILES, and FORWARD_FILE_MASK variables.
pub fn gen_bitboard_data() {
    unsafe {
        EDGES = ALL_SQUARES
            .iter()
            .filter(|x| {
                x.get_rank() == Rank::First
                    || x.get_rank() == Rank::Eighth
                    || x.get_file() == ChessFile::A
                    || x.get_file() == ChessFile::H
            })
            .fold(EMPTY, |v, s| v | BitBoard::from_square(*s));
        for i in 0..8 {
            RANKS[i] = ALL_SQUARES
                .iter()
                .filter(|x| x.get_rank().to_index() == i)
                .fold(EMPTY, |v, s| v | BitBoard::from_square(*s));
            FILES[i] = ALL_SQUARES
                .iter()
                .filter(|x| x.get_file().to_index() == i)
                .fold(EMPTY, |v, s| v | BitBoard::from_square(*s));
            ADJACENT_FILES[i] = ALL_SQUARES
                .iter()
                .filter(|y| {
                    ((y.get_file().to_index() as i8) == (i as i8) - 1)
                        || ((y.get_file().to_index() as i8) == (i as i8) + 1)
                })
                .fold(EMPTY, |v, s| v | BitBoard::from_square(*s));
        }

        // Generate FORWARD_FILE_MASK[color][square]
        // For each square and color, the mask covers the same file + adjacent files
        // on all ranks ahead of the square (not including the square's own rank).
        for sq in ALL_SQUARES.iter() {
            let file = sq.get_file().to_index();
            let rank = sq.get_rank().to_index();
            let file_mask = FILES[file] | ADJACENT_FILES[file];

            // White (color index 0): ranks ahead = higher rank indices
            let mut white_ahead = EMPTY;
            for r in (rank + 1)..8 {
                white_ahead |= RANKS[r];
            }
            FORWARD_FILE_MASK[0][sq.to_index()] = file_mask & white_ahead;

            // Black (color index 1): ranks ahead = lower rank indices
            let mut black_ahead = EMPTY;
            for r in 0..rank {
                black_ahead |= RANKS[r];
            }
            FORWARD_FILE_MASK[1][sq.to_index()] = file_mask & black_ahead;
        }
    }
}

// Write the FILES array to the specified file.
pub fn write_bitboard_data(f: &mut File) {
    unsafe {
        write!(f, "const FILES: [BitBoard; 8] = [\n").unwrap();
        for i in 0..8 {
            write!(f, "    BitBoard({}),\n", FILES[i].0).unwrap();
        }
        write!(f, "];\n").unwrap();
        write!(f, "const ADJACENT_FILES: [BitBoard; 8] = [\n").unwrap();
        for i in 0..8 {
            write!(f, "    BitBoard({}),\n", ADJACENT_FILES[i].0).unwrap();
        }
        write!(f, "];\n").unwrap();
        write!(f, "const RANKS: [BitBoard; 8] = [\n").unwrap();
        for i in 0..8 {
            write!(f, "    BitBoard({}),\n", RANKS[i].0).unwrap();
        }
        write!(f, "];\n").unwrap();
        write!(f, "/// What are all the edge squares on the `BitBoard`?\n").unwrap();
        write!(
            f,
            "pub const EDGES: BitBoard = BitBoard({});\n",
            EDGES.0
        )
        .unwrap();
        write!(f, "const FORWARD_FILE_MASK: [[BitBoard; 64]; 2] = [\n").unwrap();
        for color in 0..2 {
            write!(f, "    [\n").unwrap();
            for sq in 0..64 {
                write!(f, "        BitBoard({}),\n", FORWARD_FILE_MASK[color][sq].0).unwrap();
            }
            write!(f, "    ],\n").unwrap();
        }
        write!(f, "];\n").unwrap();
    }
}
