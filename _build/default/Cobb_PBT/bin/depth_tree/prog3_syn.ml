open Combinators

let rec depth_tree_gen =
 fun s ->
  let xccc3 = sizecheck s in
  match xccc3 with
  | true -> Leaf
  | false ->
      freq_gen s
        ~base_case:(fun _ -> Leaf)
        ~recursive_case:(fun _ ->
          let (ss : int) = subs s in
          let (lt : int tree) = depth_tree_gen ss in
          let (rt : int tree) = depth_tree_gen ss in
          let (n : int) = int_gen () in
          Node (n, lt, rt))
