open Combinators
let rec duplicate_list_gen (s : int) (x : int) : int list =
  if sizecheck s then [] else int_list_gen ()