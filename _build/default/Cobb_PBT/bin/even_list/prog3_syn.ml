open Combinators

let rec even_list_gen =
 fun s ->
  let xccc10 = sizecheck s in
  match xccc10 with
  | true ->
      let xccc11 = [] in
      let xccc12 = int_gen () in
      let xccc13 = double xccc12 in
      xccc13 :: xccc11
  | false ->
      freq_gen s
        ~base_case:(fun _ ->
          let xccc14 = [] in
          let xccc15 = int_gen () in
          let xccc16 = double xccc15 in
          xccc16 :: xccc14)
        ~recursive_case:(fun _ ->
          let idx2ccc0 = s in
          let idx5ccc0 = subs idx2ccc0 in
          let idx11ccc3 = even_list_gen idx5ccc0 in
          let idx1ccc0 = int_gen () in
          let idx8ccc0 = double idx1ccc0 in
          idx8ccc0 :: idx11ccc3)
