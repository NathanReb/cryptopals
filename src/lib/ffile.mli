module Infile : sig
  type t

  val of_string : string -> (t, string) result
end

val with_in_line_sequence :
  Infile.t ->
  (string Sequence.t -> 'a) ->
  'a
