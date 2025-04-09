open Combinators

let rec even_list_gen (s : int) : int list =
  if sizecheck s then int_list_gen ()
  else if bool_gen () then [ double (int_gen ()) ]
  else int_list_gen ()
