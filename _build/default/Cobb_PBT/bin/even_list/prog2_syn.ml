open Combinators

let rec even_list_gen =
 fun s ->
  let xccc11 = sizecheck s in
  match xccc11 with
  | true ->
      let xccc12 = [] in
      let xccc13 = int_gen () in
      let xccc14 = double xccc13 in
      xccc14 :: xccc12
  | false ->
      freq_gen s
        ~base_case:(fun _ ->
          let idx0ccc3 =[] in
          let idx1ccc0 = int_gen () in
          let idx8ccc0 = double idx1ccc0 in
          idx8ccc0 :: idx0ccc3)
        ~recursive_case:(fun _ ->
          let xccc15 = subs s in
          let xccc16 = even_list_gen xccc15 in
          let xccc17 = int_gen () in
          let xccc18 = double xccc17 in
          xccc18 :: xccc16)
