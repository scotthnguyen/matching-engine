open Base

let order ?(timestamp=0) id side price qty =
  Order.create ~id ~side ~price ~qty ~timestamp

let check_fills fills n msg =
  Alcotest.(check int) msg n (List.length fills)

let test_exact_cross () =
  let book = Book.empty in
  let sell = order 1 Order.Side.Sell 100 50 in
  let (book, _) = Matching_engine.submit_limit_order book sell in
  let buy  = order 2 Order.Side.Buy  100 50 in
  let (book', fills) = Matching_engine.submit_limit_order book buy in
  check_fills fills 1 "exact cross: one fill";
  Alcotest.(check int) "fill qty"   50  (List.hd_exn fills).Fill.qty;
  Alcotest.(check int) "fill price" 100 (List.hd_exn fills).Fill.price;
  Alcotest.(check (option int)) "bid side empty" None
    (Option.map (Book.best_bid book') ~f:Order.price);
  Alcotest.(check (option int)) "ask side empty" None
    (Option.map (Book.best_ask book') ~f:Order.price)

let test_partial_fill () =
  let book = Book.empty in
  let sell = order 1 Order.Side.Sell 100 30 in
  let (book, _) = Matching_engine.submit_limit_order book sell in
  let buy  = order 2 Order.Side.Buy  100 50 in
  let (book', fills) = Matching_engine.submit_limit_order book buy in
  check_fills fills 1 "partial fill: one fill";
  Alcotest.(check int) "fill qty" 30 (List.hd_exn fills).Fill.qty;
  (match Book.best_bid book' with
   | None -> Alcotest.fail "Expected resting bid after partial fill"
   | Some o ->
     Alcotest.(check int) "remaining bid qty" 20 (Order.remaining_qty o));
  Alcotest.(check (option int)) "ask side empty" None
    (Option.map (Book.best_ask book') ~f:Order.price)

let test_no_cross () =
  let book = Book.empty in
  let buy  = order 1 Order.Side.Buy  99  50 in
  let sell = order 2 Order.Side.Sell 101 50 in
  let (book,  fills_b) = Matching_engine.submit_limit_order book buy  in
  let (book', fills_s) = Matching_engine.submit_limit_order book sell in
  check_fills fills_b 0 "no cross: no fills for buy";
  check_fills fills_s 0 "no cross: no fills for sell";
  Alcotest.(check (option int)) "best bid" (Some 99)
    (Option.map (Book.best_bid book') ~f:Order.price);
  Alcotest.(check (option int)) "best ask" (Some 101)
    (Option.map (Book.best_ask book') ~f:Order.price)

let test_cancel () =
  let book = Book.empty in
  let sell = order 1 Order.Side.Sell 100 50 in
  let (book, _) = Matching_engine.submit_limit_order book sell in
  let book' = match Matching_engine.cancel_order book ~order_id:1 with
    | Some b -> b
    | None   -> Alcotest.fail "Expected Some on valid cancel"
  in
  Alcotest.(check (option int)) "ask gone after cancel" None
    (Option.map (Book.best_ask book') ~f:Order.price);
  Alcotest.(check int) "depth at 100 after cancel" 0
    (Book.depth_at book' Order.Side.Sell ~price:100)

let test_cancel_nonexistent () =
  let result = Matching_engine.cancel_order Book.empty ~order_id:999 in
  Alcotest.(check bool) "cancel nonexistent returns None" true (Option.is_none result)

let test_price_time_priority () =
  let book = Book.empty in
  let sell1 = order ~timestamp:0 1 Order.Side.Sell 100 10 in
  let sell2 = order ~timestamp:1 2 Order.Side.Sell 100 10 in
  let (book, _) = Matching_engine.submit_limit_order book sell1 in
  let (book, _) = Matching_engine.submit_limit_order book sell2 in
  let buy   = order 3 Order.Side.Buy 100 10 in
  let (book', fills) = Matching_engine.submit_limit_order book buy in
  check_fills fills 1 "price-time: one fill";
  Alcotest.(check int) "first fill is against sell1"
    1 (List.hd_exn fills).Fill.sell_order_id;
  (match Book.best_ask book' with
   | None -> Alcotest.fail "Expected sell2 still resting"
   | Some o ->
     Alcotest.(check int) "remaining sell id"  2  (Order.id o);
     Alcotest.(check int) "remaining sell qty" 10 (Order.remaining_qty o))

let test_market_order_full_fill () =
  let book = Book.empty in
  let sell = order 1 Order.Side.Sell 100 50 in
  let (book, _) = Matching_engine.submit_limit_order book sell in
  let (book', fills) = Matching_engine.submit_market_order book Order.Side.Buy ~qty:30 in
  check_fills fills 1 "market order: one fill";
  Alcotest.(check int) "market fill qty" 30 (List.hd_exn fills).Fill.qty;
  (match Book.best_ask book' with
   | None -> Alcotest.fail "Expected partial sell remaining"
   | Some o ->
     Alcotest.(check int) "remaining ask qty" 20 (Order.remaining_qty o))

let test_market_order_remainder_discarded () =
  let book = Book.empty in
  let sell = order 1 Order.Side.Sell 100 10 in
  let (book, _) = Matching_engine.submit_limit_order book sell in
  let (book', fills) = Matching_engine.submit_market_order book Order.Side.Buy ~qty:50 in
  check_fills fills 1 "market: one fill";
  Alcotest.(check int) "market fill qty" 10 (List.hd_exn fills).Fill.qty;
  Alcotest.(check (option int)) "no resting bid" None
    (Option.map (Book.best_bid book') ~f:Order.price)

let test_multi_level_match () =
  let book = Book.empty in
  let s1 = order 1 Order.Side.Sell 100 20 in
  let s2 = order 2 Order.Side.Sell 101 30 in
  let (book, _) = Matching_engine.submit_limit_order book s1 in
  let (book, _) = Matching_engine.submit_limit_order book s2 in
  let buy = order 3 Order.Side.Buy 105 40 in
  let (book', fills) = Matching_engine.submit_limit_order book buy in
  check_fills fills 2 "multi-level: two fills";
  Alcotest.(check int) "first fill qty"    20  (List.nth_exn fills 0).Fill.qty;
  Alcotest.(check int) "first fill price"  100 (List.nth_exn fills 0).Fill.price;
  Alcotest.(check int) "second fill qty"   20  (List.nth_exn fills 1).Fill.qty;
  Alcotest.(check int) "second fill price" 101 (List.nth_exn fills 1).Fill.price;
  (match Book.best_ask book' with
   | None -> Alcotest.fail "Expected partial sell remaining"
   | Some o ->
     Alcotest.(check int) "remaining ask qty" 10 (Order.remaining_qty o))

let test_order_invalid_qty () =
  Alcotest.check_raises "zero qty raises"
    (Failure "Order quantity must be positive")
    (fun () -> ignore (Order.create ~id:1 ~side:Order.Side.Buy ~price:100 ~qty:0 ~timestamp:0))

let test_order_status_transitions () =
  let o  = Order.create ~id:1 ~side:Order.Side.Buy  ~price:100 ~qty:10 ~timestamp:0 in
  let o' = Order.fill ~filled_qty:10 o in
  Alcotest.(check bool) "fully filled not resting" false (Order.is_resting o');
  Alcotest.check_raises "cannot fill a filled order"
    (Failure "Cannot fill an already-filled order")
    (fun () -> ignore (Order.fill ~filled_qty:1 o'));
  let o2  = Order.create ~id:2 ~side:Order.Side.Sell ~price:100 ~qty:5 ~timestamp:0 in
  let o2' = Order.cancel o2 in
  Alcotest.(check bool) "canceled not resting" false (Order.is_resting o2');
  Alcotest.check_raises "cannot cancel twice"
    (Failure "Order is already canceled")
    (fun () -> ignore (Order.cancel o2'))

let () =
  let open Alcotest in
  run "Matching Engine" [
    "order", [
      test_case "invalid qty raises"       `Quick test_order_invalid_qty;
      test_case "status transitions"       `Quick test_order_status_transitions;
    ];
    "matching", [
      test_case "exact cross"              `Quick test_exact_cross;
      test_case "partial fill"             `Quick test_partial_fill;
      test_case "no cross"                 `Quick test_no_cross;
      test_case "cancel resting order"     `Quick test_cancel;
      test_case "cancel nonexistent"       `Quick test_cancel_nonexistent;
      test_case "price-time priority"      `Quick test_price_time_priority;
      test_case "market order full fill"   `Quick test_market_order_full_fill;
      test_case "market order discard rem" `Quick test_market_order_remainder_discarded;
      test_case "multi-level match"        `Quick test_multi_level_match;
    ];
  ]
