open Combinators

let rec even_list_gen =
 fun s ->
  let xccc7 = sizecheck s in
  match xccc7 with
  | true ->
      let idx0ccc9 = [] in
      let idx1ccc1 = int_gen () in
      let idx12ccc0 = double idx1ccc1 in
      idx12ccc0 :: idx0ccc9
  | false ->
      freq_gen s
        ~base_case:(fun _ ->
          let idx0ccc6 = [] in
          let idx1ccc0 = int_gen () in
          let idx10ccc0 = double idx1ccc0 in
          idx10ccc0 :: idx0ccc6)
        ~recursive_case:(fun _ ->
          let xccc8 = subs s in
          let xccc9 = even_list_gen xccc8 in
          let xccc10 = int_gen () in
          let xccc11 = double xccc10 in
          xccc11 :: xccc9)
