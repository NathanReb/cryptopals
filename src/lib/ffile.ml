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

let read_as_line_stream in_channel =
  Stream.from @@
  fun _ ->
  try Some (input_line in_channel)
  with End_of_file -> None

let with_in_line_stream infile f =
  let in_channel = open_in infile in
  let line_stream = read_as_line_stream in_channel in
  let ret = f line_stream in
  close_in in_channel;
  ret
