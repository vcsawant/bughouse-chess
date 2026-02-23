# bughouse-chess

A Rust library for bughouse chess move generation, forked from [jordanbray/chess](https://github.com/jordanbray/chess).

## What It Does

This library provides fast, legal move generation for **bughouse chess** — a 2v2 chess variant where captured pieces transfer to the teammate's reserve for placement (drops) on their board.

Built on top of the battle-tested `chess` crate's bitboard engine, it adds:

- **Reserves**: per-color piece reserves with add/remove/count/iterate
- **Drop moves**: legal drop generation with check-aware filtering (drops must block check when in single check; no drops in double check)
- **BFEN notation**: extended FEN with `[reserves]` brackets and `~` promoted-piece markers
- **Promoted-piece tracking**: a bitboard tracks which pieces were promoted (they demote to pawns on capture in bughouse)
- **Drop-aware checkmate detection**: checkmate is only declared when no possible drop could block the check — distant sliding checks are `Ongoing` because a partner could send a piece to interpose
- **Capture tracking**: `make_move_with_capture()` returns what was captured and whether it was a promoted piece

Standard chess legality is fully preserved: pins, check evasion, castling restrictions, en passant, and all 27 upstream perft tests pass unchanged.

## BFEN Format

BFEN extends standard FEN with two additions:

- **`~` suffix** on promoted pieces: `Q~` means a promoted queen (demotes to pawn on capture)
- **`[reserves]` brackets** after piece placement: `[QNPqp]` means white has Q+N+P, black has Q+P in reserve
- Empty reserves: `[]`
- Canonical reserve order: `QRBNPqrbnp`

Example: `rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[] w KQkq - 0 1`

## Examples

### Legal Move Generation

```rust
use bughouse_chess::{Board, MoveGen, EMPTY};

let board = Board::default();
let mut movegen = MoveGen::new_legal(&board);
assert_eq!(movegen.len(), 20);
```

### Drop Moves

```rust
use bughouse_chess::{Board, MoveGen, Piece, Color};
use std::str::FromStr;

let mut board = Board::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1").unwrap();
board.add_to_reserve(Color::White, Piece::Knight);
let drops = MoveGen::drop_moves(&board);
assert_eq!(drops.len(), 62); // knight can drop on all empty squares
```

### Making a Drop

```rust
use bughouse_chess::{Board, Piece, Square, Color};
use std::str::FromStr;

let mut board = Board::from_str("k7/8/8/8/8/8/8/4K3 w - - 0 1").unwrap();
board.add_to_reserve(Color::White, Piece::Knight);
let new_board = board.make_drop_new(Piece::Knight, Square::from_str("e4").unwrap()).unwrap();
assert_eq!(new_board.piece_on(Square::from_str("e4").unwrap()), Some(Piece::Knight));
```

### Capture Tracking

```rust
use bughouse_chess::{Board, ChessMove, Square, Piece};
use std::str::FromStr;

let board = Board::from_str("k7/8/8/8/3p4/4N3/8/4K3 w - - 0 1").unwrap();
let capture = ChessMove::new(
    Square::from_str("e3").unwrap(),
    Square::from_str("d4").unwrap(),
    None,
);
let (new_board, cap_info) = board.make_move_with_capture(capture);
let (piece, was_promoted) = cap_info.unwrap();
assert_eq!(piece, Piece::Pawn);
assert!(!was_promoted);
```

### BFEN Parsing

```rust
use bughouse_chess::{Board, BoardBuilder, Piece, Color};
use std::str::FromStr;
use std::convert::TryInto;

let builder = BoardBuilder::from_str(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[QNPqp] w KQkq - 0 1"
).unwrap();
let board: Board = (&builder).try_into().unwrap();
assert_eq!(board.reserves()[Color::White.to_index()].count(Piece::Queen), 1);
```

## Checkmate Detection

In bughouse, checkmate detection differs from standard chess because a partner can send pieces at any time:

- **Unblockable checks** (knight, adjacent queen/rook, double check with no king escape) → `Checkmate`
- **Blockable sliding checks** (rook/bishop/queen from distance with empty interposition squares) → `Ongoing` (partner could send a piece to block)
- **No legal moves, not in check** → `Stalemate`

```rust
use bughouse_chess::{Board, BoardBuilder};
use bughouse_chess::board::BoardStatus;
use std::str::FromStr;
use std::convert::TryInto;

// Adjacent queen mate — unblockable
let board: Board = BoardBuilder::from_str("8/8/8/8/8/1K6/Q7/k7 b - - 0 1")
    .unwrap().try_into().unwrap();
assert_eq!(board.status(), BoardStatus::Checkmate);
```

## Compile-time Options

Use `RUSTFLAGS="-C target-cpu=native"` for best performance (enables popcnt, ctzl, and BMI2 where available).

## Upstream

Forked from [jordanbray/chess](https://github.com/jordanbray/chess). All 27 upstream perft tests are preserved and pass, validating that standard chess legality is intact.
