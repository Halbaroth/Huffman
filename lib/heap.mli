type 'a t

exception Empty

val mk: dummy:'a -> int -> 'a t
val insert: 'a t -> 'a -> int -> unit
val delete_min: 'a t -> 'a * int
val size: 'a t -> int
