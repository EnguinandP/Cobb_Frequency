open Combinators
let rec sorted_list_gen (s : int) (x : int) : int list =
  if sizecheck s then int_list_gen ()
  else
    let (y : int) = int_gen () in
    if x <= y then
      let (size2 : int) = subs s in
      let (l : int list) = sorted_list_gen size2 y in
      let (l2 : int list) = y :: l in
      l2
    else raise BailOut