open Combinators

let rec sized_list_gen =
 fun s ->
  let xccc3 = sizecheck s in
  match xccc3 with
  | true -> []
  | false ->
      freq_gen s
        ~base_case:(fun _ -> [])
        ~recursive_case:(fun _ ->
          let idx1 = int_gen () in
          let idx2 = subs s in
          let idx3 = sized_list_gen idx2 in
          idx1 :: idx3)
