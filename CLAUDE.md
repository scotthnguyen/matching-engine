# matching-engine

OCaml limit order book matching engine targeting Jane Street engineering standards.

## Environment setup

```bash
opam switch create 5.2.0   # one-time
eval $(opam env --switch=5.2.0)
opam install -y dune base core core_bench alcotest qcheck qcheck-alcotest ppx_jane stdio
```

Always prefix build/test/exec commands with `eval $(opam env --switch=5.2.0)`.

## Build & test

```bash
dune build                            # compile everything
dune test test/test_book.exe          # unit tests (Alcotest)
dune test test/test_properties.exe    # property-based tests (qcheck)
dune test                             # all tests
dune exec bench/bench_matching.exe -- -ascii -quota 3   # benchmarks
printf "SELL,100,50\nBUY,100,30\n" | dune exec bin/main.exe   # CLI
```

## Module layout

```
lib/
  order.ml/mli          — opaque Order.t, Side.t, Status.t; enforces fill/cancel transitions
  fill.ml/mli           — Fill.t record (buy_order_id, sell_order_id, price, qty)
  book.ml/mli           — purely functional Book.t backed by Base.Map int -> Level.t list
  matching_engine.ml/mli — submit_limit_order, submit_market_order, cancel_order
bin/main.ml             — line-delimited CLI replay
test/test_book.ml       — unit tests
test/test_properties.ml — qcheck property tests
bench/bench_matching.ml — core_bench throughput + latency benchmarks
```

Library uses `(wrapped false)` so all modules are top-level: `Order`, `Book`, `Fill`, `Matching_engine`.

## Key design decisions

- **int cents, not float** — avoids accumulated rounding errors across fills
- **`Order.t` is opaque** — only `Order.create`, `Order.fill`, `Order.cancel` can touch status; illegal transitions raise
- **Purely functional `Book.t`** — all operations return new values; no mutable state
- **Bid price key = -price** so `Map.min_elt` always returns best bid (highest price)
- **Market order remainder is discarded**, not rested — documented policy choice
- **id_index in each side_book** — enables O(log n) cancel without scanning all levels

## Commit style

Short, imperative messages. No AI tool references anywhere in the repo.
