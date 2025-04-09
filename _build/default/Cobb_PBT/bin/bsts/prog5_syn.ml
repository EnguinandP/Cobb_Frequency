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
        | true -> (
            let xccc9 = int_range lo in
            let x = xccc9 hi in
            let idx1617 = bool_gen () in
            match idx1617 with
            | true ->
                let idx1ccc2 = d in
                let idx204ccc0 = subs idx1ccc2 in
                let idx208 = size_bst_gen idx204ccc0 in
                let idx8 = x in
                let idx209 = idx208 idx8 in
                let idx3ccc35 = hi in
                let idx210ccc9 = idx209 idx3ccc35 in
                let idx229 = size_bst_gen idx204ccc0 in
                let idx2ccc37 = lo in
                let idx230 = idx229 idx2ccc37 in
                let idx231ccc11 = idx230 idx8 in
                Node (idx8, idx231ccc11, idx210ccc9)
            | false ->
                let idx1ccc2 = d in
                let idx204ccc0 = subs idx1ccc2 in
                let idx232 = size_bst_gen idx204ccc0 in
                let idx2ccc38 = lo in
                let idx233 = idx232 idx2ccc38 in
                let idx3ccc41 = hi in
                idx233 idx3ccc41)
        | false -> Leaf)
