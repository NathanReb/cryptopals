type 'a t = 'a option

let default def = function
    Some a -> a
  | None -> def

let bind opt f =
  match opt with
    | None -> None
    | Some a -> f a

let (>>?) = bind

let map f opt =
  match opt with
    | None -> None
    | Some a -> Some (f a)

let (>>!) opt f = map f opt
