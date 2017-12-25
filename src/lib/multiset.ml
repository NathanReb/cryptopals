module type S = sig
  type elm
  type t

  val empty : t
  val add : elm -> t -> t
  val mul : elm -> t -> int
  val fold : (elm -> int -> 'a -> 'a) -> t -> 'a -> 'a
end

module Make(X:Map.OrderedType) = struct
  module M = CCMap.Make(X)

  type elm = X.t
  type t = int M.t

  let empty = M.empty
  let add elm t =
    match M.get elm t with
      | Some i -> M.add elm (i + 1) t
      | None -> M.add elm 1 t

  let mul elm t = M.get_or ~default:0 elm t
  let fold = M.fold
end
