exception Overflow

type t = { value : int64; overflow : bool }

val ok : int64 -> t

val neg : int64 -> t
val succ : int64 -> t
val pred : int64 -> t

val add : int64 -> int64 -> t
val sub : int64 -> int64 -> t
val mul : int64 -> int64 -> t
