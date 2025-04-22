open Combinators
open Frequency_combinators
let rec sized_list_gen_thunkified = fun s ->
  let (x_0) = frequency_gen_list (fun _ -> []) in
  let (z) =
    x_0
      (fun _ ->
         let (x_2) = subs s in
         let (x_3) = sized_list_gen_thunkified x_2 in
         let (x_4) = int_gen () in x_4 :: x_3) in
  z