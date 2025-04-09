open Combinators

let rec depth_tree_gen =
 fun s ->
  let xccc4 = sizecheck s in
  match xccc4 with
  | true -> Leaf
  | false ->
      freq_gen s
        ~base_case:(fun _ -> Leaf)
        ~recursive_case:(fun _ ->
          let ss = subs s in
          let lt = depth_tree_gen ss in
          let rt = depth_tree_gen ss in
          let n = int_gen () in
          Node (n, lt, rt))
