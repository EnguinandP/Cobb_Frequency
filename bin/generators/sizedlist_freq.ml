open Combinators
open Frequency_combinators
let rec sized_list_gen = fun s ->
  let (x_2) = sizecheck s in
  match x_2 with
  | true -> []
  | false ->
      let (w0) = get_weight_idx 0 in
      let (w1) = get_weight_idx 1 in
      let (base_case) = frequency_gen_list (w0, (fun _ -> [])) in
      let (recursive_case) =
        base_case
          (w1,
            (fun _ ->
               let (x_4) = subs s in
               let (x_5) = sized_list_gen x_4 in
               let (x_6) = int_gen () in x_6 :: x_5)) in
      recursive_case