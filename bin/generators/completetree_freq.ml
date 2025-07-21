open Combinators
open Frequency_combinators
let rec complete_tree_gen = fun s ->
  let (x_69) = sizecheck s in
  match x_69 with
  | true -> Leaf
  | false ->
      let (s1) = subs s in
      let (lt) = complete_tree_gen s1 in
      let (rt) = complete_tree_gen s1 in
      let (n) = int_gen () in Node (n, lt, rt)