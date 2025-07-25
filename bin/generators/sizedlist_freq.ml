open Combinators
let rec sized_list_gen = fun s ->
  let (x_2) = sizecheck s in
  match x_2 with
  | true -> []
  | false ->
      let (size) = freq_gen s in
      let (base_case) = size ~base_case: (fun _ -> []) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (x_4) = subs s in
             let (x_5) = sized_list_gen x_4 in
             let (x_6) = int_gen () in x_6 :: x_5) in
      recursive_case