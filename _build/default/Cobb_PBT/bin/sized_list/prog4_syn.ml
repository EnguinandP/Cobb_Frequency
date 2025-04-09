open Combinators

let rec sized_list_gen =
 fun s ->
  let xccc7 = sizecheck s in
  match xccc7 with
  | true -> []
  | false ->
      freq_gen s
        ~base_case:(fun _ -> [])
        ~recursive_case:(fun _ ->
          let xccc9 = subs s in
          let xccc10 = sized_list_gen xccc9 in
          let xccc11 = int_gen () in
          xccc11 :: xccc10)
