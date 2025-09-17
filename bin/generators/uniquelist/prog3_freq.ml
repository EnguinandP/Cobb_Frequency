open Combinators
open Frequency_combinators
let rec unique_list_gen = fun s ->
  let (xccc6) = sizecheck s in
  match xccc6 with
  | true -> []
  | false ->
      let (xccc7) = subs s in
      let (l) = unique_list_gen xccc7 in
      let (x) = int_gen () in
      let (xccc8) = list_mem l in
      let (xccc9) = xccc8 x in
      (match xccc9 with
       | true -> raise BailOut
       | false -> let (idx8) = l in let (idx9) = x in idx9 :: idx8)