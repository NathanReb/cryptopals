type t
[@@deriving eq,ord,show]

val empty : t

val english : t

val of_list : (char * float) list -> t

val of_string : string -> t

val freq : t -> char -> float

val most_frequent_char : t -> char option

(** Very basic distance function between the given lexicon
    and a reference one. Just sums the distance between freq of
    each characters. *)
val distance : reference: t -> t -> float
