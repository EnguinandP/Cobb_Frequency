let rec sized_list_gen (s : int) : int list =
  let x = bool_gen () in
  match x with
  | true -> []
  | false -> [1]

