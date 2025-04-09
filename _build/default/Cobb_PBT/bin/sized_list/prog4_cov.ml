open Combinators

let rec sized_list_gen (s : int) : int list =
  if sizecheck s then int_list_gen ()
  else
    freq_gen s
      ~base_case:(fun _ -> int_list_gen ())
      ~recursive_case:(fun _ -> int_gen () :: sized_list_gen (subs s))
