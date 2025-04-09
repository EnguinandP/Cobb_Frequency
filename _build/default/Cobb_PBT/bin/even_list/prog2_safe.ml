open Combinators

let rec even_list_gen (s : int) : int list =
  if sizecheck s then [ double (int_gen ()) ]
  else
    freq_gen s
      ~base_case:(fun _ -> [ 0 ])
      ~recursive_case:(fun _ -> double (int_gen ()) :: even_list_gen (subs s))
