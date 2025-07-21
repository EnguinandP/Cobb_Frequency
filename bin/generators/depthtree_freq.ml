open Combinators
open Frequency_combinators
let rec depth_tree_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> Leaf
  | false ->
      let (w0) = get_weight_idx 0 in
      let (w1) = get_weight_idx 1 in
      let (base_case) = frequency_gen_list (w0, (fun _ -> Leaf)) in
      let (recursive_case) =
        base_case
          (w1,
            (fun _ ->
               let (ss) = subs s in
               let (lt) = depth_tree_gen ss in
               let (rt) = depth_tree_gen ss in
               let (n) = int_gen () in Node (n, lt, rt))) in
      recursive_case