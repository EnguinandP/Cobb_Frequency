open Combinators
let rec sized_list_gen (s : int) : int list =
  if sizecheck s then int_list_gen () else if bool_gen () then [] else int_list_gen ()

