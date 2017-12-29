type t = float Cchar.Map.t
[@@deriving eq,ord]

let show t =
  [%show: (char * float) list] @@ Cchar.Map.bindings t

let pp fmt t =
  Format.pp_print_string fmt @@ show t

let of_list =
  List.fold_left (fun acc (chr, freq) -> Cchar.Map.add chr freq acc) Cchar.Map.empty

let empty = Cchar.Map.empty

let freq t char = Cchar.Map.get_or ~default:0. char t

let add_char_count mset s =
  Sstring.fold (fun acc c -> Cchar.Multiset.add c acc) mset s

let of_char_count_and_total char_count total =
  let total = float total in
  Cchar.Multiset.fold
    (fun c i acc -> Cchar.Map.add c (float i /. total) acc)
    char_count
    Cchar.Map.empty

let of_string s =
  let total = String.length s in
  let char_count = add_char_count Cchar.Multiset.empty s in
  of_char_count_and_total char_count total

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

let of_sequence sequence =
  let total, char_count =
    Sequence.fold
      (fun (total, char_count) s -> (total + String.length s, add_char_count char_count s))
      (0, Cchar.Multiset.empty)
      sequence
  in
  of_char_count_and_total char_count total

let of_file infile =
  Ffile.with_in_line_sequence infile of_sequence

let english =
  of_list
    [ (' ', 0.14753722795)
    ; ('"', 0.00526918671249)
    ; ('%', 0.000229095074456)
    ; ('\'', 0.00217640320733)
    ; ('(', 0.00137457044674)
    ; (')', 0.00137457044674)
    ; (',', 0.00996563573883)
    ; ('-', 0.00309278350515)
    ; ('.', 0.00641466208477)
    ; ('/', 0.000114547537228)
    ; ('0', 0.000916380297824)
    ; ('1', 0.000687285223368)
    ; ('2', 0.000229095074456)
    ; ('3', 0.000343642611684)
    ; ('4', 0.000343642611684)
    ; ('5', 0.000229095074456)
    ; ('6', 0.00057273768614)
    ; ('7', 0.000114547537228)
    ; ('8', 0.000458190148912)
    ; (':', 0.00057273768614)
    ; (';', 0.000114547537228)
    ; ('A', 0.0028636884307)
    ; ('B', 0.00114547537228)
    ; ('C', 0.00229095074456)
    ; ('D', 0.000916380297824)
    ; ('E', 0.00355097365407)
    ; ('F', 0.00114547537228)
    ; ('G', 0.00057273768614)
    ; ('H', 0.00148911798396)
    ; ('I', 0.00114547537228)
    ; ('J', 0.000458190148912)
    ; ('K', 0.000687285223368)
    ; ('L', 0.00171821305842)
    ; ('M', 0.00103092783505)
    ; ('N', 0.00126002290951)
    ; ('O', 0.00137457044674)
    ; ('P', 0.000916380297824)
    ; ('Q', 0.000343642611684)
    ; ('R', 0.000916380297824)
    ; ('S', 0.0020618556701)
    ; ('T', 0.0028636884307)
    ; ('U', 0.00057273768614)
    ; ('V', 0.00057273768614)
    ; ('W', 0.000687285223368)
    ; ('X', 0.000458190148912)
    ; ('Y', 0.000458190148912)
    ; ('Z', 0.00057273768614)
    ; ('a', 0.057273768614)
    ; ('b', 0.0117983963345)
    ; ('c', 0.0290950744559)
    ; ('d', 0.0255441008018)
    ; ('e', 0.105612829324)
    ; ('f', 0.0227949599084)
    ; ('g', 0.0176403207331)
    ; ('h', 0.0316151202749)
    ; ('i', 0.0530355097365)
    ; ('j', 0.00126002290951)
    ; ('k', 0.00343642611684)
    ; ('l', 0.0355097365407)
    ; ('m', 0.0187857961054)
    ; ('n', 0.0548682703322)
    ; ('o', 0.0512027491409)
    ; ('p', 0.0136311569301)
    ; ('q', 0.00733104238259)
    ; ('r', 0.05578465063)
    ; ('s', 0.053264604811)
    ; ('t', 0.072852233677)
    ; ('u', 0.0250859106529)
    ; ('v', 0.00652920962199)
    ; ('w', 0.0108820160367)
    ; ('x', 0.00343642611684)
    ; ('y', 0.0153493699885)
    ; ('z', 0.00217640320733)
    ]
