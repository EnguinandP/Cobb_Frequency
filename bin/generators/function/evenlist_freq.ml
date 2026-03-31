open Combinators
open Frequency_combinators
let rec even_list_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true ->
      let (x_1) = [] in
      let (x_2) = int_gen () in let (x_3) = double x_2 in x_3 :: x_1
  | false ->
      let (w_base) = get_weight_idx 0 in
      let (w_recursive) = get_weight_idx 1 in
      let (base_case) =
        frequency_gen_list
          (w_base,
            (fun _ ->
               let (x_5) = [] in
               let (x_6) = int_gen () in let (x_7) = double x_6 in x_7 :: x_5)) in
      let (recursive_case) =
        base_case
          (w_recursive,
            (fun _ ->
               let (x_8) = subs s in
               let (x_9) = even_list_gen x_8 in
               let (x_10) = int_gen () in
               let (x_11) = double x_10 in x_11 :: x_9)) in
      recursive_case