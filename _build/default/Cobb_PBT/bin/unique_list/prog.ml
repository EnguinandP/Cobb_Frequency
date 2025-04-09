open Combinators

let rec unique_list_gen (s : int) : int list =
  if sizecheck s then []
  else
    let (l : int list) = unique_list_gen (subs s) in
    let (x : int) = int_gen () in
    if list_mem l x then begin raise BailOut end else x :: l
