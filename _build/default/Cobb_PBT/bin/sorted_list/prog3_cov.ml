open Combinators
let rec sorted_list_gen (s : int) (x : int) : int list =
  if sizecheck s then int_list_gen ()
  else
    let (y : int) = int_gen () in
    if x <= y then int_list_gen () else raise BailOut

