let rec sized_list_gen = fun (s:int) : int list ->
  let (xccc7 : bool) = sizecheck s in
  match xccc7 with
  | true -> []
  | false ->
      let (xccc8 : bool) = bool_gen () in
      (match xccc8 with
       | true -> []
       | false ->
           let (xccc9 : int) = subs s in
           let (xccc10 : int list) = sized_list_gen xccc9 in
           let (xccc11 : int) = int_gen () in xccc11 :: xccc10)