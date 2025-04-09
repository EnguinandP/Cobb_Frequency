open Combinators

let rec size_bst_gen (d : int) (lo : int) (hi : int) : int tree =
  if sizecheck d then tree_gen ()
  else if incr lo < hi then
    let (x : int) = int_range lo hi in
    freq_gen d
      ~base_case:(fun _ -> Leaf)
      ~recursive_case:(fun _ ->
        let (lt : int tree) = size_bst_gen (subs d) lo x in
        let (rt : int tree) = size_bst_gen (subs d) x hi in
        Node (x, lt, rt))
  else Leaf
