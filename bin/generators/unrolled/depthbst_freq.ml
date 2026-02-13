open Combinators
open Frequency_combinators
let rec size_bst_gen = fun d ->
  fun lo ->
    fun hi ->
      let (x_0) = sizecheck d in
      match x_0 with
      | true -> Leaf
      | false ->
          let (x_1) = incr lo in
          let (x_2) = x_1 < hi in
          (match x_2 with
           | true ->
               let (x_3) = int_range lo in
               let (x) = x_3 hi in
               let (w_base) = get_weight_idx 0 in
               let (w_recursive) = get_weight_idx 1 in
               let (base_case) = frequency_gen_list (w_base, (fun _ -> Leaf)) in
               let (recursive_case) =
                 base_case
                   (w_recursive,
                     (fun _ ->
                        let (sd) = subs d in
                        (* let (x_6) = size_bst_gen x_5 in
                        let (x_7) = x_6 lo in (* lo *)
                        let (lt) = x_7 x in hi *)

                        let lt = 
                        let (x_0) = sizecheck sd in
                        match x_0 with
                        | true -> Leaf
                        | false ->
                            let (x_1) = incr lo in
                            let (x_2) = x_1 < x in
                            (match x_2 with
                            | true ->
                                let (x_3) = int_range lo in
                                let (sx) = x_3 x in
                                let (w_base) = get_weight_idx 2 in
                                let (w_recursive) = get_weight_idx 3 in
                                let (base_case) = frequency_gen_list (w_base, (fun _ -> Leaf)) in
                                let (recursive_case) =
                                  base_case
                                    (w_recursive,
                                      (fun _ ->
                                          let (x_5) = subs sd in
                                          let (x_6) = size_bst_gen x_5 in
                                          let (x_7) = x_6 lo in
                                          let (slt) = x_7 sx in
                                          
                                          let (x_8) = subs sd in
                                          let (x_9) = size_bst_gen x_8 in
                                          let (x_10) = x_9 sx in
                                          let (srt) = x_10 x in Node (sx, slt, srt))) in
                                recursive_case
                            | false -> Leaf)

                        in
                        let (sd) = subs d in

                        let rt =
                        let (x_0) = sizecheck sd in
                        match x_0 with
                        | true -> Leaf
                        | false ->
                            let (x_1) = incr x in
                            let (x_2) = x_1 < hi in
                            (match x_2 with
                            | true ->
                                let (x_3) = int_range x in
                                let (sx) = x_3 hi in
                                let (w_base) = get_weight_idx 4 in
                                let (w_recursive) = get_weight_idx 5 in
                                let (base_case) = frequency_gen_list (w_base, (fun _ -> Leaf)) in
                                let (recursive_case) =
                                  base_case
                                    (w_recursive,
                                      (fun _ ->
                                          let (x_5) = subs sd in
                                          let (x_6) = size_bst_gen x_5 in
                                          let (x_7) = x_6 x in
                                          let (slt) = x_7 sx in

                                          let (x_8) = subs sd in
                                          let (x_9) = size_bst_gen x_8 in
                                          let (x_10) = x_9 sx in
                                          let (srt) = x_10 hi in Node (sx, slt, srt))) in
                                recursive_case
                            | false -> Leaf) 
                        in

                        Node (x, lt, rt))
                        
                        ) in
               recursive_case
           | false -> Leaf)