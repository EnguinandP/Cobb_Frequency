open Combinators
open Frequency_combinators
let rec sized_list_gen = fun s ->
  let (xccc7) = sizecheck s in
  match xccc7 with
  | true -> []
  | false ->
      let (xccc8) = frequency_gen () in
      (match xccc8 with
       | true -> []
       | false ->
           let (xccc9) = subs s in
           let (xccc10) = sized_list_gen xccc9 in
           let (xccc11) = int_gen () in xccc11 :: xccc10)