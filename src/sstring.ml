let hex_chars = "0123456789abcdef"
let base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

let hex_val = String.index hex_chars
let b64_val = String.index base64_chars

let hex_chr code = hex_chars.[code]
let b64_chr code = base64_chars.[code]

let internal_of_hex str =
  let open Char in
  let byte_of c c' = hex_val c' + (hex_val c) * 16 |> chr in
  let padded = if String.length str mod 2 = 0 then str else "0" ^ str in
  try
    Ok (String.init
          (String.length padded / 2)
          (fun i -> byte_of padded.[2 * i] padded.[2 * i + 1]))
  with Not_found -> Error "Invalid hex character"

let of_hex str =
  if String.length str = 0 then Ok str else internal_of_hex str

let to_hex str =
  let open Char in
  String.init
    (String.length str * 2)
    (fun i -> if i mod 2 = 0 then (code str.[i / 2]) / 16 |> hex_chr
      else (code str.[i / 2]) mod 16 |> hex_chr)


let get_opt str i = try Some str.[i] with Invalid_argument _ -> None

let of_chr_list l =
  let arr = Array.of_list l in
  String.init (Array.length arr) (Array.get arr)

let to_base64 str =
  let open Char in
  let open Option in
  let open Arith_8bit in
  let bytes_of c c_opt' c_opt'' =
    let c' = Option.default (chr 0) c_opt' in
    let c'' = Option.default (chr 0) c_opt'' in
    let c1 = code c >> 2 |> b64_chr in
    let c2 = (code c' >> 4) + (code c << 6 >> 2) |> b64_chr in
    let c3 = c_opt' >>! fun c' -> (code c'' >> 6) + (code c' << 4 >> 2) |> b64_chr in
    let c4 = c_opt'' >>! fun c'' -> code c'' << 2 >> 2 |> b64_chr in
    [c1; c2; Option.default '=' c3; Option.default '=' c4]
  in
  let get = get_opt str in
  let l = String.length str in
  let rec build_chr_list acc i =
    if i >= l then List.rev acc
    else
      let acc =
        List.rev_append
          (bytes_of str.[i] (get (i + 1)) (get (i + 2)))
          acc
      in
      build_chr_list acc (i + 3)
  in
  build_chr_list [] 0 |> of_chr_list

let xor s s' =
  let open Char in
  if String.length s <> String.length s' then
    Error "Expecting same length strings"
  else
    Ok (String.init
          (String.length s)
          (fun i -> (code s.[i]) lxor (code s'.[i]) |> chr))
