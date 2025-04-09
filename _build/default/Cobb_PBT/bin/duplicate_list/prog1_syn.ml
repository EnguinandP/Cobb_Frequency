open Combinators
let rec duplicate_list_gen = fun s ->
  fun x ->
    let (xccc7) = sizecheck s in
    match xccc7 with
    | true -> []
    | false ->
        let (xccc8) = subs s in
        let (xccc9) = duplicate_list_gen xccc8 in
        let (xccc10) = xccc9 x in x :: xccc10