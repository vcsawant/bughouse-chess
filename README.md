# bughouse-chess

A fast Rust library for **bughouse chess** move generation, forked from [jordanbray/chess](https://github.com/jordanbray/chess) and adapted for [bughouse](https://en.wikipedia.org/wiki/Bughouse_chess) rules.

## What is Bughouse Chess?

Bughouse is a 2v2 chess variant where captured pieces transfer to your partner's reserve for placement (dropping) on their board. Key rule differences from standard chess:

- **No check restriction** — the king can move into, through, or remain in check
- **King capture = game over** — instead of checkmate, you lose when your king is actually captured
- **Piece drops** — pieces from your reserve can be placed on any empty square (pawns restricted from ranks 1 and 8)
- **Promoted piece demotion** — a promoted piece (e.g. pawn→queen) reverts to a pawn when captured
- **Castling is unrestricted** — you can castle through attacked squares or while in "check"

## Quick Start

```rust
use bughouse_chess::*;

// Standard starting position
let board = Board::default();
let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();
assert_eq!(moves.len(), 20);

// Add a knight to white's reserve and generate drop moves
let mut board = Board::default();
board.add_to_reserve(Color::White, Piece::Knight);
let drops = MoveGen::drop_moves(&board);
assert_eq!(drops.len(), 32); // 32 empty squares

// Execute a drop
if let Some(new_board) = board.make_drop_new(Piece::Knight, Square::E4) {
    assert_eq!(new_board.reserves(Color::White).count(Piece::Knight), 0);
}
```

## BFEN — Bughouse FEN

This library uses BFEN, an extension of standard FEN that adds reserves and promoted-piece tracking:

```
rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[QNPqp] w KQkq - 0 1
                                                ^^^^^^
                                                reserves: white has Q,N,P; black has q,p
```

- **Reserves** appear in brackets after the piece placement: `[QRBNPqrbnp]`
- **Promoted pieces** are marked with `~` suffix: `Q~` means a promoted queen (demotes to pawn on capture)
- Empty reserves: `[]`
- Standard FEN (without brackets) is also accepted for backwards compatibility

```rust
use bughouse_chess::Board;
use std::str::FromStr;

let board = Board::from_str(
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR[QNPqp] w KQkq - 0 1"
).unwrap();

assert_eq!(board.reserves(Color::White).count(Piece::Queen), 1);
assert_eq!(board.reserves(Color::Black).count(Piece::Pawn), 1);
```

## Key API

### Board

```rust
// Create and inspect
let board = Board::default();
board.side_to_move();        // Color::White
board.status();              // BoardStatus::Ongoing or BoardStatus::KingCapturable
board.reserves(Color::White); // &Reserve

// Make moves
let new_board = board.make_move_new(chess_move);
let (new_board, capture_info) = board.make_move_with_capture(chess_move);
// capture_info: Option<(Piece, bool)> — (piece_type, was_promoted)

// Drop from reserves
let new_board = board.make_drop_new(Piece::Knight, Square::E4);
```

### MoveGen

```rust
// Legal board moves (standard piece moves + castling)
let moves: Vec<ChessMove> = MoveGen::new_legal(&board).collect();

// Drop moves from reserves
let drops: Vec<BughouseMove> = MoveGen::drop_moves(&board);
```

### BughouseMove

```rust
// Represents either a regular move or a piece drop
enum BughouseMove {
    Regular(ChessMove),
    Drop { piece: Piece, square: Square },
}

// Display: regular moves as UCI ("e2e4"), drops as "P@e4"
// Parse from string: "P@e4" or "e2e4"
```

### Reserve

```rust
let mut reserve = Reserve::new();
reserve.add(Piece::Knight);
reserve.add(Piece::Pawn);
assert_eq!(reserve.count(Piece::Knight), 1);
assert!(reserve.remove(Piece::Knight));
assert_eq!(reserve.total(), 1);
```

## Differences from upstream `chess` crate

This fork makes the following changes to support bughouse rules:

| Area | Standard Chess | Bughouse Chess |
|------|---------------|----------------|
| Check | King cannot be left in check | King can stay in check; king capture ends game |
| Pins | Pinned pieces cannot move | All pieces move freely |
| Castling | Cannot castle in/through/into check | Castling only requires empty squares |
| Move gen | Filtered by pins, checks, evasions | All pseudo-legal moves are legal |
| Game over | Checkmate or stalemate | King capture (BoardStatus::KingCapturable) |
| Reserves | N/A | Per-color piece reserves for drops |
| Drops | N/A | Place reserve pieces on empty squares |
| Promoted pieces | N/A | Tracked; demote to pawn on capture |
| Position hash | Board only | Board + reserve state |

## Building

```bash
cargo build

# Run tests
cargo test

# Recommended: enable native CPU optimizations
RUSTFLAGS="-C target-cpu=native" cargo build --release
```

## Used By

- [bughouse-engine](https://github.com/vcsawant/bughouse-engine) — Rust bughouse engine (UBI protocol)
- [bughouse](https://github.com/vcsawant/bughouse) — Phoenix LiveView bughouse platform
