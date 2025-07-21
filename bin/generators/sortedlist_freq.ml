open Combinators
open Frequency_combinators
let rec sorted_list_gen = fun s ->
  fun x ->
    let (x_55) = sizecheck s in
    match x_55 with
    | true -> []
    | false ->
        let (y) = int_gen () in
        let (x_56) = x <= y in
        (match x_56 with
         | true ->
             let (size2) = subs s in
             let (x_57) = sorted_list_gen size2 in
             let (l) = x_57 y in let (l2) = y :: l in l2
         | false -> Err)