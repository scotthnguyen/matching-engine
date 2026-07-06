# OCaml Limit Order Book

A type-safe limit order book matching engine written in OCaml, using Jane Street's own libraries (`Base`, `Core`, `core_bench`). Orders are matched under price-time priority; invariants are enforced at the type level wherever possible and verified for arbitrary inputs by a property-based test suite.

---

## Design decisions

### Integer prices, not floats

Prices are stored as integer cents. Floating-point accumulation errors are a genuine concern in trading systems: if a mid-price calculation or fill P&L is computed in floating point across millions of fills, the rounding drift is real money. Integer arithmetic eliminates the problem entirely. The minor inconvenience of working in cents rather than dollars is worth it.

### `.mli`-enforced invariants — the most important structural choice

`Order.t` is an opaque type. Its raw record fields are invisible to callers outside the `Order` module. The only way to create an order is `Order.create`, which validates quantity and price at the boundary; the only way to transition its status is `Order.fill` or `Order.cancel`, which enforce legal transitions:

```
Resting → Filled    (when fill_qty exhausts remaining_qty)
Resting → Canceled  (explicit cancel)
```

Calling `fill` on an already-`Filled` or `Canceled` order raises at the call site. This isn't a runtime check that "we hope holds" — it's a module boundary that makes the invalid states unreachable from any caller. The matching engine itself works through `Order.fill` and never touches the status field directly.

This is the single most "Jane Street style" decision in the project: use the type system and module system together to shrink the surface area where bugs can hide.

### Purely functional book

`Book.t` is immutable — every operation returns a new `t`. This makes the property-based tests straightforward (snapshot the state before an operation, check invariants on the state after) and eliminates an entire class of bugs where a partially-updated mutable structure is observed mid-operation. The performance cost over a mutable approach is real but acceptable for a matching engine that isn't in the hot path of a co-located strategy — and for a demonstration, correctness is worth more than microseconds.

### Data structure: `Base.Map` (price → FIFO queue)

Each side of the book is a `Base.Map` from price key to a list of resting orders in arrival order. Bid side uses negated prices as keys so `Map.min_elt` always returns the best bid (highest absolute price) without a separate comparator. This gives O(log n) insert/remove at a price level and O(1) best-bid/ask access via `min_elt`.

An alternative — a heap keyed by (price, timestamp) — would give the same asymptotic complexity but makes partial fills and cancels harder: you'd need a lazy-deletion tombstone scheme or an auxiliary index. The map + queue approach keeps each concern separate.

A secondary `id_index` (`order_id → price_key`) enables O(log n) cancels without scanning the book.

### Market order remainder policy

Unfilled market order quantity is discarded, not converted to a limit order. This matches the most common exchange behavior and preserves the invariant that every resting order has a defined price. The alternative — resting the remainder as an aggressive limit — would require inventing a price, which is ambiguous. The CLI and tests document this choice explicitly.

---

## What the type system enforces vs. what the tests verify

| Invariant | Enforced by |
|---|---|
| `Order.t` can only be constructed with positive price and qty | `Order.create` — raises at boundary |
| Status transitions are legal (`Resting → Filled/Canceled` only) | `Order.fill` / `Order.cancel` — raise on invalid states |
| Raw order fields are not directly mutatable by callers | `.mli` opacity |
| **No crossed book** after any sequence of operations | Property-based test (`qcheck`) |
| **No zero-qty resting orders** | Property-based test |
| **Fill prices are positive** | Property-based test |
| **Depth at any level is non-negative** | Property-based test |

The type system handles the per-order invariants. The property tests handle the emergent book-level invariants that only make sense across a session of operations — the kind of thing that's hard to encode as a type but easy to check on random inputs.

---

## Benchmark numbers

_(Run `dune exec bench/bench_matching.exe -- -ascii -quota 3` to reproduce)_

Measured on a MacBook Pro M-series, OCaml 5.2.0, `dune build` default optimization level:

| Benchmark | Time/run | Throughput |
|---|---|---|
| Throughput, 10K orders (alternating buy/sell, ~50% cross rate) | 242 µs | **41M orders/sec** |
| Throughput, 100K orders | 2,429 µs | **41M orders/sec** |
| Single `submit_limit_order`, book depth 10 | **20 ns** | — |
| Single `submit_limit_order`, book depth 100 | **20 ns** | — |
| Single `submit_limit_order`, book depth 1,000 | **21 ns** | — |
| Single `submit_limit_order`, book depth 10,000 | **21 ns** | — |

Two things stand out:

**Throughput is stable across order counts.** The book doesn't grow unboundedly when orders cross — matching clears levels — so the working set stays small and cache-warm across the full 100K run.

**Single-submit latency is flat across book depths.** All four depth measurements are within 1 ns of each other. At these depths, `log₂(10000) ≈ 14` tree comparisons × ~1–2 ns per cache-warm comparison = ~20 ns matches what we observe. The O(log n) is real but invisible at these depths on a modern CPU with hot caches.

---

## What I'd do differently with more time

**Replace the FIFO queue with `Core.Fqueue`:** The current list-as-queue does O(n) appends for the tail. `Core.Fqueue` (a functional deque) gives amortized O(1) enqueue and O(1) front access, which matters at high order rates on a single price level.

**Finer-grained quantity accounting in property tests:** The current property tests track approximate fill/cancel quantities. A complete implementation would carry an `order_id → original_qty` map through the session and assert exact conservation: `Σ submitted = Σ filled + Σ resting + Σ canceled`.

**Fills-respect-price-time-priority property:** This is the hardest property to write cleanly. It requires the generator to produce scenarios where multiple resting orders at the same price exist and then verify which one got matched. Worth doing; I scoped it to unit tests only.

**Async TCP server:** The `bin/main.ml` CLI handles file replay. An `Async` TCP server that accepts the same line protocol would make the engine usable as a live service and demonstrates Async familiarity. The matching core is already decoupled from I/O so the integration would be straightforward.

**Lock-free data structures for concurrent access:** The purely functional book is thread-safe for reads but requires external synchronization for writes. In a real exchange, the matching engine is single-threaded by design (sequenced order flow), so this isn't a bottleneck — but it's worth documenting.

---

## Building and running

```bash
# Install dependencies
opam install . --deps-only

# Build everything
dune build

# Run unit tests
dune test test/test_book.exe

# Run property-based tests
dune test test/test_properties.exe

# Run benchmarks
dune exec bench/bench_matching.exe -- -ascii

# Replay an order file
echo "SELL,100,50\nBUY,100,30\nCANCEL,1" | dune exec bin/main.exe
```

### Example order file format

```
# Simple cross
SELL,10000,50
BUY,10000,50

# Partial fill
BUY,10100,100
SELL,10100,40

# Market order
MBUY,20

# Cancel
CANCEL,1
```
