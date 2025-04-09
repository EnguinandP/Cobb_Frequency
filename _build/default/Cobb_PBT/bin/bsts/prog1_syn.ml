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
            let xccc17 = bool_gen () in
            freq_gen d
              ~base_case:(fun _ -> Leaf)
              ~recursive_case:(fun _ ->
                let (lt : int tree) = size_bst_gen (subs d) lo x in
                let (rt : int tree) = size_bst_gen (subs d) x hi in
                Node (x, lt, rt))
        | false -> Leaf)
