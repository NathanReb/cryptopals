module type S = sig
  type elm
  type t

  val empty : t
  val add : elm -> t -> t
  val mul : elm -> t -> int
  val fold : (elm -> int -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make(X:Map.OrderedType) : S with type elm = X.t
