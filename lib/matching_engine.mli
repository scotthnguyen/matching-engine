(** Core matching logic: price-time priority matching for limit and market
    orders.

    All functions are pure — they return a new [Book.t] and a list of [Fill.t]
    records rather than mutating the book in place. *)

(** [submit_limit_order book order] matches [order] against resting orders on
    the opposite side of [book] at a crossing price, then rests any unfilled
    remainder.  Returns the updated book and the list of fills generated. *)
val submit_limit_order : Book.t -> Order.t -> Book.t * Fill.t list

(** [submit_market_order book side ~qty] matches [qty] units against the best
    available prices on the opposing side.  Any unfilled remainder is
    discarded (not rested) — the caller receives fills for the matched
    portion.  Returns the updated book and fills.

    Policy choice: remainder is discarded rather than converted to a limit
    order.  This matches the most common exchange behavior and keeps the
    invariant that every resting order has a defined price. *)
val submit_market_order : Book.t -> Order.Side.t -> qty:int -> Book.t * Fill.t list

(** [cancel_order book ~order_id] removes the resting order from the book.
    Returns [None] if no resting order with that id exists. *)
val cancel_order : Book.t -> order_id:int -> Book.t option
