module Infile : sig
  type t

  val of_string : string -> (t, string) result
end

val with_in_line_stream :
  Infile.t ->
  (string Stream.t -> 'a) ->
  'a
