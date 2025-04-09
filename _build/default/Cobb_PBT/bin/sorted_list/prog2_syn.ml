open Combinators
let rec sorted_list_gen = fun s ->
  fun x ->
    let (xccc3) = sizecheck s in
    match xccc3 with
    | true -> []
    | false ->
        let (y) = int_gen () in
        let (xccc4) = x <= y in
        (match xccc4 with
         | true ->
             let (idx1) = s in
             let (idx13ccc0) = subs idx1 in
             let (idx50) = sorted_list_gen idx13ccc0 in
             let (idx6) = y in
             let (idx51ccc1) = idx50 idx6 in idx6 :: idx51ccc1
         | false -> raise BailOut)