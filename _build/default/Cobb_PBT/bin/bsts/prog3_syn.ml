open Combinators

let rec size_bst_gen =
 fun d ->
  fun lo ->
   fun hi ->
    let xccc13 = sizecheck d in
    match xccc13 with
    | true -> Leaf
    | false -> (
        let xccc14 = incr lo in
        let xccc15 = xccc14 < hi in
        match xccc15 with
        | true ->
            let xccc16 = int_range lo in
            let x = xccc16 hi in
            freq_gen d
              ~base_case:(fun _ -> Leaf)
              ~recursive_case:(fun _ ->
                let xccc18 = subs d in
                let xccc19 = size_bst_gen xccc18 in
                let xccc20 = xccc19 lo in
                let lt = xccc20 x in
                let xccc21 = subs d in
                let xccc22 = size_bst_gen xccc21 in
                let xccc23 = xccc22 x in
                let rt = xccc23 hi in
                Node (x, lt, rt))
        | false -> Leaf)
