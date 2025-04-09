open Combinators

let rec sized_list_gen =
 fun s ->
  let xccc7 = sizecheck s in
  freq_gen s
    ~base_case:(fun _ -> [])
    ~recursive_case:(fun _ ->
      let xccc8 = int_gen () in
      let xccc9 = subs s in
      xccc8 :: sized_list_gen xccc9)
