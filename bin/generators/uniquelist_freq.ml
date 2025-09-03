open Combinators
open Frequency_combinators
let rec unique_list_gen = fun s ->
  let (x_0) = sizecheck s in
  match x_0 with
  | true -> []
  | false ->
      let (x_1) = subs s in
      let (l) = unique_list_gen x_1 in
      let (x) = int_gen () in
      let (x_2) = list_mem l in
      let (x_3) = x_2 x in
      (match x_3 with | true -> raise BailOut | false -> x :: l)