open Combinators
let rec duplicate_list_gen = fun s ->
  fun x ->
    let (x_7) = sizecheck s in
    match x_7 with
    | true -> []
    | false ->
        let (x_8) = subs s in
        let (x_9) = duplicate_list_gen x_8 in let (x_10) = x_9 x in x :: x_10