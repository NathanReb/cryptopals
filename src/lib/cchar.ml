let xor c c' = Char.(chr @@ (code c) lxor (code c'))

module Map = CCMap.Make(CCChar)
