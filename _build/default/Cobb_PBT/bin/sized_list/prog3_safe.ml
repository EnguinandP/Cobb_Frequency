open Combinators

let rec sized_list_gen (s : int) : int list =
  if sizecheck s then [] else if bool_gen () then [] else []
