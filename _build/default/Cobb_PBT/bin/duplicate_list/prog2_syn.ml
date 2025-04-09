open Combinators
let rec duplicate_list_gen = fun s ->
  fun x ->
    let (xccc2) = sizecheck s in
    match xccc2 with
    | true -> []
    | false ->
        let (idx1) = s in
        let (idx6ccc0) = subs idx1 in
        let (idx7) = duplicate_list_gen idx6ccc0 in
        let (idx2) = x in let (idx8ccc1) = idx7 idx2 in idx2 :: idx8ccc1