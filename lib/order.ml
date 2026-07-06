open Base

module Side = struct
  type t = Buy | Sell [@@deriving sexp_of]

  let opposite = function Buy -> Sell | Sell -> Buy
end

module Status = struct
  type t =
    | Resting of { remaining_qty : int }
    | Filled
    | Canceled
  [@@deriving sexp_of]
end

type t = {
  id        : int;
  side      : Side.t;
  price     : int;
  qty       : int;
  timestamp : int;
  status    : Status.t;
} [@@deriving sexp_of]

let create ~id ~side ~price ~qty ~timestamp =
  if qty <= 0 then failwith "Order quantity must be positive";
  if price <= 0 then failwith "Order price must be positive";
  { id; side; price; qty; timestamp; status = Status.Resting { remaining_qty = qty } }

let id        o = o.id
let side      o = o.side
let price     o = o.price
let qty       o = o.qty
let timestamp o = o.timestamp
let status    o = o.status

let remaining_qty o =
  match o.status with
  | Status.Resting { remaining_qty } -> remaining_qty
  | Status.Filled | Status.Canceled  -> 0

let is_resting o =
  match o.status with
  | Status.Resting _ -> true
  | Status.Filled | Status.Canceled -> false

let fill ~filled_qty o =
  match o.status with
  | Status.Filled   -> failwith "Cannot fill an already-filled order"
  | Status.Canceled -> failwith "Cannot fill a canceled order"
  | Status.Resting { remaining_qty } ->
    if filled_qty <= 0 then failwith "filled_qty must be positive";
    if filled_qty > remaining_qty then failwith "filled_qty exceeds remaining quantity";
    let new_remaining = remaining_qty - filled_qty in
    let new_status =
      if new_remaining = 0 then Status.Filled
      else Status.Resting { remaining_qty = new_remaining }
    in
    { o with status = new_status }

let cancel o =
  match o.status with
  | Status.Filled   -> failwith "Cannot cancel an already-filled order"
  | Status.Canceled -> failwith "Order is already canceled"
  | Status.Resting _ -> { o with status = Status.Canceled }
