open Combinators
open Frequency_combinators
let rec depth_tree_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> Leaf
  | false ->
      let (w_base) = get_weight_idx 0 in
      let (w_recursive) = get_weight_idx 1 in
      let (c_b) = get_weight_idx 2 in
      let (c_rec) = get_weight_idx 3 in
      let (base_case) = freq_para_2_gen s c_b c_rec (w_base, (fun _ -> Leaf)) in
      let (recursive_case) =
        base_case
          (w_recursive,
            (fun _ ->
               let (ss) = subs s in
               let (lt) = depth_tree_gen ss in
               let (rt) = depth_tree_gen ss in
               let (n) = int_gen () in Node (n, lt, rt))) in
      recursive_case