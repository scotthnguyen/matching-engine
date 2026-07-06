(** A [Fill.t] represents a single matched trade between a resting order and
    an incoming order.  Fills are structured data, not just side-effects on
    the book, so callers can accumulate and audit them independently. *)

type t = {
  buy_order_id  : int;
  sell_order_id : int;
  price         : int;  (** Price of the resting order — the aggressor pays this *)
  qty           : int;
} [@@deriving sexp_of]

val pp : Format.formatter -> t -> unit
