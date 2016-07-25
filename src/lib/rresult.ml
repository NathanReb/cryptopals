type ('a, 'b) t = ('a, 'b) result

let compare ~cmp ~cmp_err r r' =
  match r, r' with
    | Ok _, Error _ -> -1
    | Error _, Ok _ -> 1
    | Error b, Error b' -> cmp_err b b'
    | Ok a, Ok a' -> cmp a a'

let equal ~eq ~eq_err r r' =
  match r, r' with
    | Ok a, Ok a' -> eq a a'
    | Error b, Error b' -> eq_err b b'
    | _ -> false

let to_string ~print ~print_err = function
  | Ok a -> Printf.sprintf "Ok %s" (print a)
  | Error b -> Printf.sprintf "Error %s" (print_err b)

let bind r f =
  match r with
    | Ok a -> f a
    | Error b -> Error b

let (>>>) = bind

let map f = function
  | Ok a -> Ok (f a)
  | Error b -> Error b

let (>>|) r f = map f r

let iter_error f = function
  | Error b -> f b
  | Ok _ -> ()
