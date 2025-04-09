open Combinators
let rec duplicate_list_gen (s : int) (x : int) : int list =
  if sizecheck s then int_list_gen () else x :: duplicate_list_gen (subs s) x
