open Combinators
open Frequency_combinators
let rec sized_list_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> []
  | false ->
      let (w0) = get_weight_idx 0 in
      let (w1) = get_weight_idx 1 in
      let (c0) = get_weight_idx 2 in
      let (c1) = get_weight_idx 3 in  
      let (base_case) = freq_para_2_gen s c0 c1 (w0, (fun _ -> [])) in
      let (recursive_case) =
        base_case
          (w1,
            (fun _ ->
               let (x_2) = subs s in
               let (x_3) = sized_list_gen x_2 in
               let (x_4) = int_gen () in x_4 :: x_3)) in
      recursive_case