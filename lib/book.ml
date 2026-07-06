open Base

(* A price level is a FIFO queue of resting orders, represented as a list
   where the head is the oldest (highest priority) order. *)
module Level = struct
  type t = Order.t list

  let empty : t = []
  let is_empty = List.is_empty

  let add (level : t) (order : Order.t) : t =
    level @ [order]

  let remove_by_id (level : t) ~id : t option =
    let filtered = List.filter level ~f:(fun o -> Order.id o <> id) in
    if List.length filtered = List.length level then None
    else Some filtered

  let update (level : t) (order : Order.t) : t =
    List.map level ~f:(fun o ->
      if Order.id o = Order.id order then order else o)

  let best (level : t) : Order.t option = List.hd level

  let total_qty (level : t) : int =
    List.fold level ~init:0 ~f:(fun acc o -> acc + Order.remaining_qty o)
end

(* Side book: Map from price_key -> Level.t
   For bids, price_key = -price (so Map.min_elt = best bid = highest price).
   For asks, price_key = price  (so Map.min_elt = best ask = lowest price).
   id_index maps order_id -> price_key for O(log n) cancels. *)
type side_book = {
  levels    : Level.t Map.M(Int).t;
  price_key : int -> int;
  id_index  : int Map.M(Int).t;
}

let empty_side ~price_key = {
  levels    = Map.empty (module Int);
  price_key;
  id_index  = Map.empty (module Int);
}

let side_add sb order =
  let key = sb.price_key (Order.price order) in
  let level =
    match Map.find sb.levels key with
    | None   -> Level.empty
    | Some l -> l
  in
  let levels   = Map.set sb.levels ~key ~data:(Level.add level order) in
  let id_index = Map.set sb.id_index ~key:(Order.id order) ~data:key in
  { sb with levels; id_index }

let side_remove sb ~order_id =
  match Map.find sb.id_index order_id with
  | None -> None
  | Some key ->
    (match Map.find sb.levels key with
     | None -> None
     | Some level ->
       (match Level.remove_by_id level ~id:order_id with
        | None -> None
        | Some new_level ->
          let levels =
            if Level.is_empty new_level then Map.remove sb.levels key
            else Map.set sb.levels ~key ~data:new_level
          in
          let id_index = Map.remove sb.id_index order_id in
          Some { sb with levels; id_index }))

let side_best sb =
  match Map.min_elt sb.levels with
  | None          -> None
  | Some (_, level) -> Level.best level

let side_update sb order =
  let key = sb.price_key (Order.price order) in
  match Map.find sb.levels key with
  | None -> sb
  | Some level ->
    let new_level = Level.update level order in
    if Level.is_empty new_level then
      let levels   = Map.remove sb.levels key in
      let id_index = Map.remove sb.id_index (Order.id order) in
      { sb with levels; id_index }
    else
      let levels = Map.set sb.levels ~key ~data:new_level in
      { sb with levels }

let side_depth_at sb ~price =
  let key = sb.price_key price in
  match Map.find sb.levels key with
  | None   -> 0
  | Some l -> Level.total_qty l

let side_snapshot sb =
  Map.fold sb.levels ~init:[] ~f:(fun ~key:_ ~data:level acc ->
    match Level.best level with
    | None -> acc
    | Some o -> (Order.price o, Level.total_qty level) :: acc)

let side_size sb =
  Map.fold sb.levels ~init:0 ~f:(fun ~key:_ ~data:level acc ->
    acc + List.length level)

(* ---- Public type ---- *)

type t = {
  bids : side_book;
  asks : side_book;
}

let sexp_of_t t =
  let all_orders sb =
    Map.fold sb.levels ~init:[] ~f:(fun ~key:_ ~data:level acc -> acc @ level)
  in
  [%sexp_of: Order.t list * Order.t list] (all_orders t.bids, all_orders t.asks)

let empty = {
  bids = empty_side ~price_key:(fun p -> -p);
  asks = empty_side ~price_key:(fun p ->  p);
}

let side_book_of t = function
  | Order.Side.Buy  -> t.bids
  | Order.Side.Sell -> t.asks

let set_side t side sb =
  match side with
  | Order.Side.Buy  -> { t with bids = sb }
  | Order.Side.Sell -> { t with asks = sb }

let add t order =
  let sb  = side_book_of t (Order.side order) in
  set_side t (Order.side order) (side_add sb order)

let remove t ~order_id =
  match side_remove t.bids ~order_id with
  | Some bids' -> Some { t with bids = bids' }
  | None ->
    match side_remove t.asks ~order_id with
    | Some asks' -> Some { t with asks = asks' }
    | None -> None

let best_bid t = side_best t.bids
let best_ask t = side_best t.asks

let update_order t order =
  let sb  = side_book_of t (Order.side order) in
  set_side t (Order.side order) (side_update sb order)

let depth_at t side ~price =
  side_depth_at (side_book_of t side) ~price

let bids_snapshot t =
  side_snapshot t.bids
  |> List.sort ~compare:(fun (p1, _) (p2, _) -> Int.compare p2 p1)

let asks_snapshot t =
  side_snapshot t.asks
  |> List.sort ~compare:(fun (p1, _) (p2, _) -> Int.compare p1 p2)

let size t = side_size t.bids + side_size t.asks
