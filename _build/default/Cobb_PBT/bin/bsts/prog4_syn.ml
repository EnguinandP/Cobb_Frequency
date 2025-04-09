open Combinators

let rec size_bst_gen =
 fun d ->
  fun lo ->
   fun hi ->
    let xccc6 = sizecheck d in
    match xccc6 with
    | true -> Leaf
    | false -> (
        let xccc7 = incr lo in
        let xccc8 = xccc7 < hi in
        match xccc8 with
        | true ->
            let xccc9 = int_range lo in
            let x = xccc9 hi in
            freq_gen d
              ~base_case:(fun _ -> Leaf)
              ~recursive_case:(fun _ ->
                let idx1 = d in
                let idx204ccc0 = subs idx1 in
                let idx208 = size_bst_gen idx204ccc0 in
                let idx8 = x in
                let idx209 = idx208 idx8 in
                let idx3 = hi in
                let idx210ccc10 = idx209 idx3 in
                let idx229 = size_bst_gen idx204ccc0 in
                let idx2 = lo in
                let idx230 = idx229 idx2 in
                let idx231ccc13 = idx230 idx8 in
                Node (idx8, idx231ccc13, idx210ccc10))
        | false -> Leaf)
