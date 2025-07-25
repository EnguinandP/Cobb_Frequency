open Combinators
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
               let (size) = freq_gen d in
               let (base_case) =
                 size ~base_case:
                   (fun _ ->
                      let (x_5) = subs d in
                      let (x_6) = size_bst_gen x_5 in
                      let (x_7) = x_6 lo in
                      let (lt) = x_7 x in
                      let (x_8) = subs d in
                      let (x_9) = size_bst_gen x_8 in
                      let (x_10) = x_9 x in
                      let (rt) = x_10 hi in Node (x, lt, rt)) in
               let (recursive_case) =
                 base_case ~recursive_case: (fun _ -> Leaf) in
               recursive_case
           | false -> Leaf)