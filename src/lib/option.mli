type 'a t = 'a option

val default : 'a -> 'a t -> 'a

val bind : 'a t -> ('a -> 'b t) -> 'b t
val (>>?) : 'a t -> ('a -> 'b t) -> 'b t

val map : ('a -> 'b) -> 'a t -> 'b t
val (>>!) : 'a t -> ('a -> 'b) -> 'b t
