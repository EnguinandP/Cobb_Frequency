open Combinators

let rec even_list_gen (s : int) : int list =
  if sizecheck s then [0] else [ double (int_gen ()) ]
