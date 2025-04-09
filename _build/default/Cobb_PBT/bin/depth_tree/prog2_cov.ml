open Combinators

let rec depth_tree_gen (s : int) : int tree =
  if sizecheck s then Leaf
  else
    freq_gen s
      ~base_case:(fun _ -> tree_gen ())
      ~recursive_case:(fun _ ->
        let (ss : int) = subs s in
        let (lt : int tree) = depth_tree_gen ss in
        let (rt : int tree) = depth_tree_gen ss in
        let (n : int) = int_gen () in
        Node (n, lt, rt))
