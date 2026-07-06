open Base

(* Returns true if [incoming] crosses [resting] — i.e. if a trade can occur. *)
let crosses ~incoming ~resting =
  match Order.side incoming with
  | Order.Side.Buy  -> Order.price incoming >= Order.price resting
  | Order.Side.Sell -> Order.price incoming <= Order.price resting

(* Match [incoming] against the book, returning (updated_book, fills, updated_incoming). *)
let rec match_order book incoming fills =
  if not (Order.is_resting incoming) then (book, fills, incoming)
  else
    let best_opposing =
      match Order.side incoming with
      | Order.Side.Buy  -> Book.best_ask book
      | Order.Side.Sell -> Book.best_bid book
    in
    match best_opposing with
    | None -> (book, fills, incoming)
    | Some resting ->
      if not (crosses ~incoming ~resting) then (book, fills, incoming)
      else begin
        let fill_qty = Int.min (Order.remaining_qty incoming) (Order.remaining_qty resting) in
        let fill_price = Order.price resting in (* resting order sets the price *)
        let buy_id, sell_id =
          match Order.side incoming with
          | Order.Side.Buy  -> Order.id incoming, Order.id resting
          | Order.Side.Sell -> Order.id resting,  Order.id incoming
        in
        let fill = Fill.{ buy_order_id = buy_id; sell_order_id = sell_id; price = fill_price; qty = fill_qty } in
        let resting'  = Order.fill ~filled_qty:fill_qty resting in
        let incoming' = Order.fill ~filled_qty:fill_qty incoming in
        (* Update book: remove resting if fully filled, else update qty *)
        let book' =
          if Order.is_resting resting' then Book.update_order book resting'
          else
            match Book.remove book ~order_id:(Order.id resting) with
            | Some b -> b
            | None   -> book
        in
        match_order book' incoming' (fill :: fills)
      end

let submit_limit_order book order =
  let (book', fills, final_order) = match_order book order [] in
  let book'' =
    if Order.is_resting final_order then Book.add book' final_order
    else book'
  in
  (book'', List.rev fills)

(* Market order: match against the book; discard any unfilled remainder. *)
let submit_market_order book side ~qty =
  if qty <= 0 then failwith "Market order quantity must be positive";
  (* Synthesize a synthetic order with an extreme price so it crosses everything. *)
  let synthetic_price =
    match side with
    | Order.Side.Buy  -> Int.max_value / 2  (* will cross any ask *)
    | Order.Side.Sell -> 1                  (* will cross any bid >= 1 cent *)
  in
  let synthetic = Order.create ~id:(-1) ~side ~price:synthetic_price ~qty ~timestamp:0 in
  let (book', fills, _final) = match_order book synthetic [] in
  (* Do not rest the remainder — market orders never rest. *)
  (book', List.rev fills)

let cancel_order book ~order_id =
  Book.remove book ~order_id
