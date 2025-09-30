open Combinators
open Frequency_combinators
let rec sorted_list_gen = fun s ->
  fun x ->
    let (x_0) = sizecheck s in
    match x_0 with
    | true -> []
    | false ->
        let (w_0) = get_weight_idx 0 in
        let (w_1) = get_weight_idx 1 in
        let (w_2) = get_weight_idx 3 in
        let (y) = nat_freq_x_gen x w_0 w_1 w_2 in
        let (x_1) = x <= y in
        (match x_1 with
         | true ->
             let (size2) = subs s in
             let (x_2) = sorted_list_gen size2 in
             let (l) = x_2 y in let (l2) = y :: l in l2
         | false -> raise BailOut)