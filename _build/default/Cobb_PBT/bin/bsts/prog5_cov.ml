open Combinators

let rec size_bst_gen (d : int) (lo : int) (hi : int) : int tree =
  if sizecheck d then tree_gen ()
  else if incr lo < hi then
    let (x : int) = int_range lo hi in
    if bool_gen () then tree_gen () else tree_gen ()
  else tree_gen ()
