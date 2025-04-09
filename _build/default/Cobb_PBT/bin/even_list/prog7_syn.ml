open Combinators

let rec even_list_gen =
 fun s ->
  let xccc2 = sizecheck s in
  match xccc2 with
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
          let idx2ccc0 = s in
          let idx5ccc0 = subs idx2ccc0 in
          let idx15ccc3 = even_list_gen idx5ccc0 in
          let idx1ccc0 = int_gen () in
          let idx10ccc0 = double idx1ccc0 in
          idx10ccc0 :: idx15ccc3)
