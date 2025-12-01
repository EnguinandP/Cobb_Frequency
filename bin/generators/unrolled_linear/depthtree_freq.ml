open Combinators
open Frequency_combinators

let rec depth_tree_gen =
 fun s ->
  let x_0 = sizecheck s in
  match x_0 with
  | true -> Leaf
  | false ->
      let w_base = get_weight_idx 0 in
      let w_recursive = get_weight_idx 1 in
      (* let base_case = frequency_gen_list (w_base, fun _ -> Leaf) in *)
      let (c_b) = get_weight_idx 2 in
      let (c_rec) = get_weight_idx 3 in
      let (base_case) = freq_para_2_gen s c_b c_rec (w_base, (fun _ -> Leaf)) in
      let recursive_case =
        base_case
          ( w_recursive,
            fun _ ->
              let ss = subs s in
              let lt =
                let x_0 = sizecheck ss in
                match x_0 with
                | true -> Leaf
                | false ->
                    let w_base2 = get_weight_idx 4 in
                    let w_recursive2 = get_weight_idx 5 in
                    let (c_b) = get_weight_idx 6 in
                    let (c_rec) = get_weight_idx 7 in
                    freq_para_2_gen
                      ss c_b c_rec
                      (w_base2, fun _ -> Leaf)
                      ( w_recursive2,
                        fun _ ->
                          let ss2 = subs ss in
                          let lt2 = depth_tree_gen ss2 in
                          let rt2 = depth_tree_gen ss2 in
                          let n2 = int_gen () in
                          Node (n2, lt2, rt2) )
              in
              let rt =
                let x_0 = sizecheck ss in
                match x_0 with
                | true -> Leaf
                | false ->
                    let w_base3 = get_weight_idx 8 in
                    let w_recursive3 = get_weight_idx 9 in
                    let (c_b) = get_weight_idx 10 in
                    let (c_rec) = get_weight_idx 11 in
                    freq_para_2_gen
                      ss c_b c_rec
                      (w_base3, fun _ -> Leaf)
                      ( w_recursive3,
                        fun _ ->
                          let ss2 = subs ss in
                          let lt2 = depth_tree_gen ss2 in
                          let rt2 = depth_tree_gen ss2 in
                          let n2 = int_gen () in
                          Node (n2, lt2, rt2) )
              in
              let n = int_gen () in
              Node (n, lt, rt) )
      in
      recursive_case
