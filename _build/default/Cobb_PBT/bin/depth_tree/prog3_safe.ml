open Combinators

let rec depth_tree_gen (s : int) : int tree =
  if sizecheck s then Leaf else if bool_gen () then Leaf else Leaf
