(** The limit order book: two sides of resting orders sorted by price-time
    priority.

    Data structure: [Base.Map] from price (int) to a FIFO queue of orders at
    that level.  Bids are keyed by negated price so that [Map.min_elt] always
    yields the best bid (highest absolute price).  Asks are keyed by raw price
    so [Map.min_elt] yields the best ask (lowest price).

    The book is purely functional — every operation returns a new [t].  This
    makes property tests straightforward (snapshot before, compare after) and
    eliminates the class of bugs where a partially-updated mutable structure is
    observed by concurrent code. *)

type t

val sexp_of_t : t -> Sexplib0.Sexp.t

val empty : t

(** Add a resting order to the book.  The order must be in [Resting] status. *)
val add : t -> Order.t -> t

(** Remove an order by id.  Returns [None] if the order is not resting on
    this side of the book. *)
val remove : t -> order_id:int -> t option

(** Best bid (highest resting buy price), if any. *)
val best_bid : t -> Order.t option

(** Best ask (lowest resting sell price), if any. *)
val best_ask : t -> Order.t option

(** Replace an existing resting order (same id, same side, same price) with
    an updated version — used after a partial fill to reduce remaining_qty. *)
val update_order : t -> Order.t -> t

(** Total resting quantity at a given price level on the given side. *)
val depth_at : t -> Order.Side.t -> price:int -> int

(** Snapshot of the entire book as (price, total_qty) pairs, bids descending,
    asks ascending. *)
val bids_snapshot : t -> (int * int) list
val asks_snapshot : t -> (int * int) list

(** Total number of resting orders (both sides). *)
val size : t -> int
