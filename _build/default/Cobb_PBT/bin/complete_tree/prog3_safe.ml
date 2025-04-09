open Combinators

let rec complete_tree_gen (s : int) : int tree =
  if sizecheck s then Leaf
  else Node (s, complete_tree_gen (s - 1), complete_tree_gen (s - 1))
