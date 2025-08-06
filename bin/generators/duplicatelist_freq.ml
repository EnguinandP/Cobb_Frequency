open Combinators
open Frequency_combinators
let rec duplicate_list_gen = fun s ->
  fun x ->
    let (x_0) = sizecheck s in
    match x_0 with
    | true -> []
    | false ->
        let (x_1) = subs s in
        let (x_2) = duplicate_list_gen x_1 in let (x_3) = x_2 x in x :: x_3