(** CLI order replay.

    Line formats (one per line):
      BUY,<price>,<qty>    — submit limit buy  (price in cents)
      SELL,<price>,<qty>   — submit limit sell
      MBUY,<qty>           — submit market buy  (remainder discarded)
      MSELL,<qty>          — submit market sell
      CANCEL,<id>          — cancel a resting order
      #...                 — comment / blank line (ignored)
*)

open Base
open Stdio

type state = {
  book    : Book.t;
  next_id : int;
}

let init = { book = Book.empty; next_id = 1 }

let parse_line line =
  let line = String.strip line in
  if String.is_empty line || Char.(line.[0] = '#') then `Skip
  else
    match String.split line ~on:',' with
    | ["BUY";   price; qty] -> `Limit (Order.Side.Buy,  Int.of_string price, Int.of_string qty)
    | ["SELL";  price; qty] -> `Limit (Order.Side.Sell, Int.of_string price, Int.of_string qty)
    | ["MBUY";  qty]        -> `Market (Order.Side.Buy,  Int.of_string qty)
    | ["MSELL"; qty]        -> `Market (Order.Side.Sell, Int.of_string qty)
    | ["CANCEL"; id]        -> `Cancel (Int.of_string id)
    | _ -> printf "WARN: unrecognized: %s\n" line; `Skip

let run_line state line =
  match parse_line line with
  | `Skip -> state
  | `Limit (side, price, qty) ->
    let order = Order.create ~id:state.next_id ~side ~price ~qty ~timestamp:state.next_id in
    let (book', fills) = Matching_engine.submit_limit_order state.book order in
    List.iter fills ~f:(fun f ->
      printf "FILL buy=%d sell=%d price=%d qty=%d\n"
        f.Fill.buy_order_id f.Fill.sell_order_id f.Fill.price f.Fill.qty);
    if List.is_empty fills then
      printf "REST id=%d side=%s price=%d qty=%d\n"
        state.next_id
        (match side with Order.Side.Buy -> "BUY" | Order.Side.Sell -> "SELL")
        price qty;
    { book = book'; next_id = state.next_id + 1 }
  | `Market (side, qty) ->
    let (book', fills) = Matching_engine.submit_market_order state.book side ~qty in
    List.iter fills ~f:(fun f ->
      printf "FILL buy=%d sell=%d price=%d qty=%d\n"
        f.Fill.buy_order_id f.Fill.sell_order_id f.Fill.price f.Fill.qty);
    { state with book = book' }
  | `Cancel id ->
    (match Matching_engine.cancel_order state.book ~order_id:id with
     | None   -> printf "WARN: cancel id=%d not found\n" id; state
     | Some b -> printf "CANCELED id=%d\n" id; { state with book = b })

let print_book_snapshot book =
  printf "\n=== Book Snapshot ===\n";
  printf "ASKS (lowest first):\n";
  List.iter (Book.asks_snapshot book) ~f:(fun (price, qty) ->
    printf "  %d @ %d\n" qty price);
  printf "BIDS (highest first):\n";
  List.iter (Book.bids_snapshot book) ~f:(fun (price, qty) ->
    printf "  %d @ %d\n" qty price);
  (match Book.best_bid book, Book.best_ask book with
   | Some bid, Some ask ->
     printf "Spread: %d\n" (Order.price ask - Order.price bid)
   | _ -> printf "(one or both sides empty)\n")

let () =
  let lines =
    let argv = Stdlib.Sys.argv in
    if Array.length argv >= 2 then In_channel.read_lines argv.(1)
    else In_channel.input_lines In_channel.stdin
  in
  let final = List.fold lines ~init ~f:run_line in
  print_book_snapshot final.book
