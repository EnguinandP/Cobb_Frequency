open Combinators

let rec depth_tree_gen =
 fun s ->
  let xccc2 = sizecheck s in
  match xccc2 with
  | true -> Leaf
  | false ->
      freq_gen s
        ~base_case:(fun _ -> Leaf)
        ~recursive_case:(fun _ ->
          let idx2 = s in
          let idx5ccc0 = subs idx2 in
          let idx7ccc8 = depth_tree_gen idx5ccc0 in
          let idx7ccc7 = depth_tree_gen idx5ccc0 in
          let idx1ccc0 = int_gen () in
          Node (idx1ccc0, idx7ccc7, idx7ccc8))
