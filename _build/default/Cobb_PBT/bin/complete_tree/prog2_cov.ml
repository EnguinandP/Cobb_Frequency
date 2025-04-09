open Combinators

let rec complete_tree_gen (s : int) : int tree =
  if sizecheck s then Leaf else tree_gen ()
