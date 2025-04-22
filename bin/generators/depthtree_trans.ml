open Combinators
open Frequency_combinators
let rec depth_tree_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> Leaf
  | false ->
      let (x_1) = frequency_gen () in
      (match x_1 with
       | true -> Leaf
       | false ->
           let (ss) = subs s in
           let (lt) = depth_tree_gen ss in
           let (rt) = depth_tree_gen ss in
           let (n) = int_gen () in Node (n, lt, rt))