open Combinators
let rec size_bst_gen = fun d ->
  fun lo ->
    fun hi ->
      let (x_58) = sizecheck d in
      match x_58 with
      | true -> Leaf
      | false ->
          let (x_59) = incr lo in
          let (x_60) = x_59 < hi in
          (match x_60 with
           | true ->
               let (x_61) = int_range lo in
               let (x) = x_61 hi in
               let (size) = freq_gen d in
               let (base_case) =
                 size ~base_case:
                   (fun _ ->
                      let (x_63) = subs d in
                      let (x_64) = size_bst_gen x_63 in
                      let (x_65) = x_64 lo in
                      let (lt) = x_65 x in
                      let (x_66) = subs d in
                      let (x_67) = size_bst_gen x_66 in
                      let (x_68) = x_67 x in
                      let (rt) = x_68 hi in Node (x, lt, rt)) in
               let (recursive_case) =
                 base_case ~recursive_case: (fun _ -> Leaf) in
               recursive_case
           | false -> Leaf)