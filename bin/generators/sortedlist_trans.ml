open Combinators
open Frequency_combinators
let rec sorted_list_gen = fun s ->
  fun x ->
    let (x_0) = sizecheck s in
    match x_0 with
    | true -> []
    | false ->
        let (y) = int_gen () in
        let (x_1) = x <= y in
        (match x_1 with
         | true ->
             let (size2) = subs s in
             let (x_2) = sorted_list_gen size2 in
             let (l) = x_2 y in let (l2) = y :: l in l2
         | false -> Err)