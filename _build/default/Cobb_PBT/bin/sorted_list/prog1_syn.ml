open Combinators
let rec sorted_list_gen = fun s ->
  fun x ->
    let (xccc6) = sizecheck s in
    match xccc6 with
    | true -> []
    | false ->
        let (y) = int_gen () in
        let (xccc7) = x <= y in
        (match xccc7 with
         | true ->
             let (size2) = subs s in
             let (xccc8) = sorted_list_gen size2 in
             let (l) = xccc8 y in let (l2) = y :: l in l2
         | false -> raise BailOut)