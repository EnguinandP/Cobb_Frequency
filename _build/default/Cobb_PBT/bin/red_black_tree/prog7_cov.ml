open Combinators

let rec rbtree_gen (inv : int) (color : bool) (h : int) : int rbtree =
  if sizecheck h then
    if color then default_rbtree_gen ()
    else default_rbtree_gen ()
  else if color then
    default_rbtree_gen ()
  else
    default_rbtree_gen ()
