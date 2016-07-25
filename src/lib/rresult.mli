type ('a, 'b) t = ('a, 'b) result

val compare :
  cmp: ('a -> 'a -> int) ->
  cmp_err: ('b -> 'b -> int) ->
  ('a, 'b) t -> ('a, 'b) t -> int

val equal :
  eq: ('a -> 'a -> bool) ->
  eq_err: ('b -> 'b -> bool) ->
  ('a, 'b) t -> ('a, 'b) t -> bool

val to_string :
  print: ('a -> string) ->
  print_err: ('b -> string) -> ('a, 'b) t -> string

val bind : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t
val (>>>) : ('a, 'b) t -> ('a -> ('c, 'b) t) -> ('c, 'b) t

val map : ('a -> 'c) -> ('a, 'b) t -> ('c, 'b) t
val (>>|) : ('a, 'b) t -> ('a -> 'c) -> ('c, 'b) t

val iter_error : ('b -> unit) -> ('a, 'b) t -> unit
