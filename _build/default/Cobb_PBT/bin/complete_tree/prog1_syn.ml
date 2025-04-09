open Combinators

let rec complete_tree_gen = fun s ->
  let (xccc3) = sizecheck s in
  match xccc3 with
  | true -> Leaf
  | false ->
      let (s1) = subs s in
      let (lt) = complete_tree_gen s1 in
      let (rt) = complete_tree_gen s1 in
      let (n) = int_gen () in Node (n, lt, rt)