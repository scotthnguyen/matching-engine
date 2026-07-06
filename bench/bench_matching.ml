open Core
open Core_bench

let make_alternating_orders n =
  Array.init n ~f:(fun i ->
    let side = if i % 2 = 0 then Order.Side.Buy else Order.Side.Sell in
    let price = if i % 2 = 0 then 100 + (i % 5) else 100 - (i % 5) in
    Order.create ~id:i ~side ~price:(Int.max 1 price) ~qty:10 ~timestamp:i)

let bench_throughput_n n =
  let orders = make_alternating_orders n in
  Bench.Test.create ~name:(Printf.sprintf "throughput/%dk" (n / 1000)) (fun () ->
    let _book =
      Array.fold orders ~init:Book.empty ~f:(fun book order ->
        fst (Matching_engine.submit_limit_order book order))
    in
    ())

let bench_single_submit_at_depth depth =
  let book =
    Array.init depth ~f:(fun i ->
      Order.create ~id:(i + 100_000) ~side:Order.Side.Sell ~price:200 ~qty:1 ~timestamp:i)
    |> Array.fold ~init:Book.empty ~f:(fun b o ->
        fst (Matching_engine.submit_limit_order b o))
  in
  let order = Order.create ~id:99999 ~side:Order.Side.Buy ~price:100 ~qty:10 ~timestamp:0 in
  Bench.Test.create ~name:(Printf.sprintf "single_submit/depth_%d" depth) (fun () ->
    ignore (Matching_engine.submit_limit_order book order))

let () =
  Command_unix.run (Bench.make_command [
    bench_throughput_n 10_000;
    bench_throughput_n 100_000;
    bench_single_submit_at_depth 10;
    bench_single_submit_at_depth 100;
    bench_single_submit_at_depth 1_000;
    bench_single_submit_at_depth 10_000;
  ])
