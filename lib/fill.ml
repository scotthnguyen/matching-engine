open Base

type t = {
  buy_order_id  : int;
  sell_order_id : int;
  price         : int;
  qty           : int;
} [@@deriving sexp_of]

let pp fmt t =
  Stdlib.Format.fprintf fmt "Fill{buy=%d sell=%d price=%d qty=%d}"
    t.buy_order_id t.sell_order_id t.price t.qty
