open Combinators
open Frequency_combinators
let rec sized_list_gen_thunkified (s : int) : int list =
  let z : int list  = frequency_gen_list
        (fun (_ : Random.State.t) : int list -> []) 
        (fun (_ : Random.State.t) : int list ->
              let (x_2 : int) = subs s in
              let (x_3 : int list) = sized_list_gen_thunkified x_2 in
              let (x_4 : int) = int_gen () in 
              x_4 :: x_3)
        in z
