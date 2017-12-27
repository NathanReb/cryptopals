type t = float Cchar.Map.t
[@@deriving eq,ord]

let show t =
  [%show: (char * float) list] @@ Cchar.Map.bindings t

let pp fmt t =
  Format.pp_print_string fmt @@ show t

let of_list =
  List.fold_left (fun acc (chr, freq) -> Cchar.Map.add chr freq acc) Cchar.Map.empty

let empty = Cchar.Map.empty

let english =
  of_list
    [ ('a', 8.167)
    ; ('b', 1.492)
    ; ('c', 2.782)
    ; ('d', 4.253)
    ; ('e', 12.702)
    ; ('f', 2.228)
    ; ('g', 2.015)
    ; ('h', 6.094)
    ; ('i', 6.966)
    ; ('j', 0.153)
    ; ('k', 0.772)
    ; ('l', 4.025)
    ; ('m', 2.406)
    ; ('n', 6.749)
    ; ('o', 7.507)
    ; ('p', 1.929)
    ; ('q', 0.095)
    ; ('r', 5.987)
    ; ('s', 6.327)
    ; ('t', 9.056)
    ; ('u', 2.758)
    ; ('v', 0.978)
    ; ('w', 2.361)
    ; ('x', 0.150)
    ; ('y', 1.974)
    ; ('z', 0.074)
    ]

let freq t char = Cchar.Map.get_or ~default:0. char t

let of_string s =
  let module M = Multiset.Make(Char) in
  let total = float @@ String.length s in
  let char_count = Sstring.fold (fun acc c -> M.add c acc) M.empty s in
  M.fold
    (fun c i acc -> Cchar.Map.add c (float i /. total) acc)
    char_count
    Cchar.Map.empty

let most_frequent_char t =
  Cchar.Map.fold
    ( fun char freq -> function
        | None -> Some (char, freq)
        | Some (c_max, f_max)
          ->
            if freq > f_max then
              Some (char, freq)
            else
              Some (c_max, f_max)
    )
    t
    None
  |> CCOpt.map fst

let distance ~reference lexicon =
  Cchar.Map.fold
    ( fun c f acc ->
        let ref_freq = freq reference c in
        acc +. (abs_float @@ ref_freq -. f)
    )
    lexicon
    0.
