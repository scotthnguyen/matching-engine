open Base

(* ---- Generators ---- *)

let gen_price = QCheck.Gen.(int_range 95 105)
let gen_qty   = QCheck.Gen.(int_range 1 100)
let gen_side  = QCheck.Gen.(map (fun b -> if b then Order.Side.Buy else Order.Side.Sell) bool)

type action =
  | Submit_limit  of { side : Order.Side.t; price : int; qty : int }
  | Submit_market of { side : Order.Side.t; qty : int }
  | Cancel        of int  (* index into live_ids list at run time *)

let gen_action live_count =
  QCheck.Gen.(
    let cancel_gen =
      if live_count = 0 then return (Cancel 0)
      else map (fun i -> Cancel i) (int_range 0 (live_count - 1))
    in
    oneof_weighted [
      (6, map3 (fun side price qty -> Submit_limit  { side; price; qty }) gen_side gen_price gen_qty);
      (2, map2 (fun side qty       -> Submit_market { side; qty })         gen_side gen_qty);
      (2, cancel_gen);
    ])

let gen_actions n =
  QCheck.Gen.(
    let rec go acc live_count steps =
      if steps = 0 then return (List.rev acc)
      else
        gen_action live_count >>= fun action ->
        let live_count' =
          match action with
          | Submit_limit _  -> live_count + 1
          | Submit_market _ -> live_count
          | Cancel _        -> Int.max 0 (live_count - 1)
        in
        go (action :: acc) live_count' (steps - 1)
    in
    go [] 0 n)

(* ---- Session state ---- *)

type session = {
  book     : Book.t;
  live_ids : int list;
  next_id  : int;
}

let init_session = { book = Book.empty; live_ids = []; next_id = 1 }

let run_action s action =
  match action with
  | Submit_limit { side; price; qty } ->
    let order = Order.create ~id:s.next_id ~side ~price ~qty ~timestamp:s.next_id in
    let (book', fills) = Matching_engine.submit_limit_order s.book order in
    let filled_qty = List.fold fills ~init:0 ~f:(fun acc f -> acc + f.Fill.qty) in
    let live_ids' =
      if qty - filled_qty > 0 then s.next_id :: s.live_ids
      else s.live_ids
    in
    { book = book'; live_ids = live_ids'; next_id = s.next_id + 1 }
  | Submit_market { side; qty } ->
    let (book', _fills) = Matching_engine.submit_market_order s.book side ~qty in
    { s with book = book' }
  | Cancel idx ->
    (match s.live_ids with
     | [] -> s
     | ids ->
       let id_to_cancel = List.nth_exn ids (idx % List.length ids) in
       let live_ids' = List.filter ids ~f:(fun i -> i <> id_to_cancel) in
       (match Matching_engine.cancel_order s.book ~order_id:id_to_cancel with
        | None    -> { s with live_ids = live_ids' }
        | Some b' -> { s with book = b'; live_ids = live_ids' }))

let run_session actions =
  List.fold actions ~init:init_session ~f:run_action

(* ---- Invariants ---- *)

let no_crossed_book book =
  match Book.best_bid book, Book.best_ask book with
  | Some bid, Some ask -> Order.price bid < Order.price ask
  | _                  -> true

let no_zero_qty_levels book =
  let ok snapshots = List.for_all snapshots ~f:(fun (_, qty) -> qty > 0) in
  ok (Book.bids_snapshot book) && ok (Book.asks_snapshot book)

let non_negative_size book = Book.size book >= 0

(* ---- Properties ---- *)

let arb_actions n =
  QCheck.make (gen_actions n)
    ~print:(fun actions -> Printf.sprintf "<%d actions>" (List.length actions))

let prop_no_crossed_book =
  QCheck.Test.make ~name:"no crossed book after any sequence" ~count:1000
    (arb_actions 50)
    (fun actions -> no_crossed_book (run_session actions).book)

let prop_no_zero_qty =
  QCheck.Test.make ~name:"no zero-qty levels after any sequence" ~count:1000
    (arb_actions 50)
    (fun actions -> no_zero_qty_levels (run_session actions).book)

let prop_non_negative_size =
  QCheck.Test.make ~name:"book size non-negative" ~count:1000
    (arb_actions 50)
    (fun actions -> non_negative_size (run_session actions).book)

let prop_fill_prices_positive =
  QCheck.Test.make ~name:"all fill prices and quantities are positive" ~count:500
    (arb_actions 40)
    (fun actions ->
      let (_, ok) =
        List.fold actions ~init:(init_session, true) ~f:(fun (s, ok) action ->
          let (book', new_fills) =
            match action with
            | Submit_limit { side; price; qty } ->
              let order = Order.create ~id:s.next_id ~side ~price ~qty ~timestamp:s.next_id in
              Matching_engine.submit_limit_order s.book order
            | Submit_market { side; qty } ->
              Matching_engine.submit_market_order s.book side ~qty
            | Cancel _ ->
              (s.book, [])
          in
          let s' = run_action { s with book = book' } (match action with
            | Submit_limit a  -> Submit_limit a
            | Submit_market a -> Submit_market a
            | Cancel i        -> Cancel i) in
          let fills_valid = List.for_all new_fills ~f:(fun f -> f.Fill.price > 0 && f.Fill.qty > 0) in
          (s', ok && fills_valid))
      in
      ok)

let prop_invariant_every_step =
  QCheck.Test.make ~name:"no-crossed-book holds at every intermediate step" ~count:500
    (arb_actions 30)
    (fun actions ->
      let (_, ok) =
        List.fold actions ~init:(init_session, true) ~f:(fun (s, ok) action ->
          let s' = run_action s action in
          (s', ok && no_crossed_book s'.book))
      in
      ok)

(* ---- Runner ---- *)

let () =
  let suite = [
    prop_no_crossed_book;
    prop_no_zero_qty;
    prop_non_negative_size;
    prop_fill_prices_positive;
    prop_invariant_every_step;
  ] in
  Alcotest.run "Property-based invariants"
    [ "invariants", List.map suite ~f:QCheck_alcotest.to_alcotest ]
