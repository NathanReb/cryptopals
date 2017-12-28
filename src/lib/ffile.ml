module Infile = struct
  type t = string

  let of_string name =
    let t = CCIO.File.make name in
    if not (CCIO.File.exists t) then
      Error (Printf.sprintf "File %s doesn't exist" name)
    else if CCIO.File.is_directory t then
      Error (Printf.sprintf "%s is a directory" name)
    else
      Ok t
end

let rec read_as_line_sequence in_channel k =
  let line = try Some (input_line in_channel) with End_of_file -> None in
  match line with
    | None -> ()
    | Some l -> k l; read_as_line_sequence in_channel k

let with_in_line_sequence infile f =
  let in_channel = open_in infile in
  let line_sequence = read_as_line_sequence in_channel in
  let ret = f line_sequence in
  close_in in_channel;
  ret
