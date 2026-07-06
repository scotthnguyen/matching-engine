(** Type-safe order representation.

    Prices are represented as integer cents to avoid floating-point rounding
    errors — a real concern in trading systems where accumulated rounding across
    millions of fills would produce incorrect P&L.

    The [status] variant encodes legal state transitions at the type level:
    only a [Resting] order has remaining quantity; a [Filled] or [Canceled]
    order cannot be further modified.  Callers interact only through the
    functions below, so the raw record is never directly constructable from
    outside this module. *)

module Side : sig
  type t = Buy | Sell [@@deriving sexp_of]

  val opposite : t -> t
end

module Status : sig
  (** Encodes the lifecycle of an order.  [Resting] holds the mutable
      remaining quantity; once transitioned to [Filled] or [Canceled] the
      order is immutable and no further matching may occur on it. *)
  type t =
    | Resting of { remaining_qty : int }
    | Filled
    | Canceled
  [@@deriving sexp_of]
end

type t [@@deriving sexp_of]

val create : id:int -> side:Side.t -> price:int -> qty:int -> timestamp:int -> t

val id        : t -> int
val side      : t -> Side.t
val price     : t -> int
val qty       : t -> int
val timestamp : t -> int
val status    : t -> Status.t

(** Returns the remaining quantity if [Resting], 0 otherwise. *)
val remaining_qty : t -> int

(** [fill ~filled_qty order] returns a new order with [remaining_qty] reduced
    by [filled_qty].  Raises if [order] is not [Resting] or if [filled_qty]
    exceeds remaining quantity. *)
val fill : filled_qty:int -> t -> t

(** [cancel order] returns a new order in [Canceled] status.
    Raises if [order] is already [Filled] or [Canceled]. *)
val cancel : t -> t

val is_resting : t -> bool
