type t

val create: (int * char) list -> t
val encode: t -> Bytes.t -> bool list option
val decode: t -> bool list -> Bytes.t option
