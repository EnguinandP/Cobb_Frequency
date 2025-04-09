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
          let idx0 = int_gen () in
          let idx1 = sized_list_gen (subs s) in
          idx0 :: idx1)
