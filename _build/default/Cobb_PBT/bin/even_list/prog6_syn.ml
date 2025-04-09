open Combinators

let rec even_list_gen =
 fun s ->
  let xccc6 = sizecheck s in
  match xccc6 with
  | true ->
      let idx0ccc6 = [] in
      let idx1ccc0 = int_gen () in
      let idx10ccc0 = double idx1ccc0 in
      idx10ccc0 :: idx0ccc6
  | false ->
      freq_gen s
        ~base_case:(fun _ ->
          let xccc7 = [] in
          let xccc8 = int_gen () in
          let xccc9 = double xccc8 in
          xccc9 :: xccc7)
        ~recursive_case:(fun _ ->
          let idx2ccc1 = s in
          let idx7ccc0 = subs idx2ccc1 in
          let idx17ccc3 = even_list_gen idx7ccc0 in
          let idx1ccc1 = int_gen () in
          let idx12ccc0 = double idx1ccc1 in
          idx12ccc0 :: idx17ccc3)
