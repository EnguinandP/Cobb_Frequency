open Combinators
open Frequency_combinators
let rec rbtree_gen = fun inv ->
  fun color ->
    fun h ->
      let (x_0) = sizecheck h in
      match x_0 with
      | true ->
          (match color with
           | true -> Rbtleaf
           | false ->
               let (w0) = get_weight_idx 0 in
               let (w1) = get_weight_idx 1 in
               let (base_case) = frequency_gen_list (w0, (fun _ -> Rbtleaf)) in
               let (recursive_case) =
                 base_case
                   (w1,
                     (fun _ ->
                        let (x_2) = Rbtleaf in
                        let (x_3) = int_gen () in
                        let (x_4) = Rbtleaf in Rbtnode (true, x_4, x_3, x_2))) in
               recursive_case)
      | false ->
          (match color with
           | true ->
               let (x_5) = subs inv in
               let (x_6) = rbtree_gen x_5 in
               let (x_7) = x_6 false in
               let (x_8) = subs h in
               let (lt2) = x_7 x_8 in
               let (x_9) = subs inv in
               let (x_10) = rbtree_gen x_9 in
               let (x_11) = x_10 false in
               let (x_12) = subs h in
               let (rt2) = x_11 x_12 in
               let (x_13) = int_gen () in Rbtnode (false, lt2, x_13, rt2)
           | false ->
               let (w2) = get_weight_idx 2 in
               let (w3) = get_weight_idx 3 in
               let (base_case) =
                 frequency_gen_list
                   (w2,
                     (fun _ ->
                        let (x_14) = subs inv in
                        let (x_15) = rbtree_gen x_14 in
                        let (x_16) = x_15 true in
                        let (lt3) = x_16 h in
                        let (x_17) = subs inv in
                        let (x_18) = rbtree_gen x_17 in
                        let (x_19) = x_18 true in
                        let (rt3) = x_19 h in
                        let (x_20) = int_gen () in
                        Rbtnode (true, lt3, x_20, rt3))) in
               let (recursive_case) =
                 base_case
                   (w3,
                     (fun _ ->
                        let (x_21) = subs inv in
                        let (x_22) = subs x_21 in
                        let (x_23) = rbtree_gen x_22 in
                        let (x_24) = x_23 false in
                        let (x_25) = subs h in
                        let (lt4) = x_24 x_25 in
                        let (x_26) = subs inv in
                        let (x_27) = subs x_26 in
                        let (x_28) = rbtree_gen x_27 in
                        let (x_29) = x_28 false in
                        let (x_30) = subs h in
                        let (rt4) = x_29 x_30 in
                        let (x_31) = int_gen () in
                        Rbtnode (false, lt4, x_31, rt4))) in
               recursive_case)