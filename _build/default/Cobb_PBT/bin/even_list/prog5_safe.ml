open Combinators

let rec even_list_gen (s : int) : int list =
  if sizecheck s then [ double (int_gen ()) ]
  else [0]
