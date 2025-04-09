open Combinators
let rec unique_list_gen = fun s ->
  let (xccc7) = sizecheck s in
  match xccc7 with
  | true -> []
  | false ->
      let (xccc8) = subs s in
      let (l) = unique_list_gen xccc8 in
      let (x) = int_gen () in
      let (xccc9) = list_mem l in
      let (xccc10) = xccc9 x in
      (match xccc10 with | true -> raise BailOut | false -> x :: l)