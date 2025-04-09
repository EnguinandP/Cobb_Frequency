open Combinators

let rec sized_list_gen =
 fun s ->
  let xccc2 = sizecheck s in
  match xccc2 with
  | true -> []
  | false ->
      freq_gen s
        ~base_case:(fun _ -> [])
        ~recursive_case:(fun _ ->
          let xccc0 = int_gen () in
          let xccc1 = sized_list_gen (subs s) in
          xccc0 :: xccc1)
