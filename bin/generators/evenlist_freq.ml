open Combinators
let rec even_list_gen = fun s ->
  let (x_43) = sizecheck s in
  match x_43 with
  | true ->
      let (x_44) = [] in
      let (x_45) = int_gen () in let (x_46) = double x_45 in x_46 :: x_44
  | false ->
      let (size) = freq_gen s in
      let (base_case) =
        size ~base_case:
          (fun _ ->
             let (x_48) = [] in
             let (x_49) = int_gen () in
             let (x_50) = double x_49 in x_50 :: x_48) in
      let (recursive_case) =
        base_case ~recursive_case:
          (fun _ ->
             let (x_51) = subs s in
             let (x_52) = even_list_gen x_51 in
             let (x_53) = int_gen () in
             let (x_54) = double x_53 in x_54 :: x_52) in
      recursive_case