let int_size = Sys.word_size - 1

let comp = int_size - 8

let trunc i = (i lsl comp) lsr comp

let sl i s = (i lsl (comp + s)) lsr comp
let (<<) = sl

let sr i s = (i lsl comp) lsr (comp + s)
let (>>) = sr
