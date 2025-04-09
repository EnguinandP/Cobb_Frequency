open Combinators
let rec sized_list_gen (s : int) : int list = if sizecheck s then [] else int_list_gen ()

