open Combinators
open Frequency_combinators
let rec sized_list_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> []
  | false ->
      let (base_case) = frequency_gen_list (fun _ -> []) in
      let (recursive_case) =
        base_case
          (fun _ ->
             let (x_2) = subs s in
             let (x_3) = sized_list_gen x_2 in
             let (x_4) = int_gen () in x_4 :: x_3) in
      recursive_case