open Combinators

let rec complete_tree_gen (s : int) : int tree =
  if sizecheck s then tree_gen ()
  else
    let (s1 : int) = subs s in
    let (lt : int tree) = complete_tree_gen s1 in
    let (rt : int tree) = complete_tree_gen s1 in
    let (n : int) = int_gen () in
    Node (n, lt, rt)
