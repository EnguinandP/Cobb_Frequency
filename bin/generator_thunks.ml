let a = ref 1

(** duplicatelist generator with size 10 and x = 5 *)
let uniquelist () =
  let size = 1000 in
  try Some (Generators.Uniquelist_freq.unique_list_gen size)
  with Combinators.BailOut ->
    a := !a + 1;
    Printf.printf "bail %d\n" !a;
    None

(** duplicatelist generator with size 10 and x = 5 *)
let duplicatelist () =
  let size = 10 in
  let x = 5 in
  Generators.Duplicatelist_freq.duplicate_list_gen size x

(** evenlist generator with size 10 *)
let evenlist () =
  let size = 10 in
  Generators.Evenlist_freq.even_list_gen size

(** sizedlist generator with size 10 *)
let sizedlist () =
  let size = 10 in
  Generators.Sizedlist_freq.sized_list_gen size

(** sortedlist generator with size 10 and x = 5 *)
let sortedlist () =
  let size = 10 in
  let x = 5 in
  try Some (Generators.Sortedlist_freq.sorted_list_gen size x)
  with Combinators.BailOut -> None

(** rb tree generator inv - tree height is 4 or 5 color - red h - black height
    is 2 (patrick uses max height 6) *)
let rbtree () =
  let height = 2 in
  let color = true in
  (* true = red *)
  let inv = if color then 2 * height else (2 * height) + 1 in
  Generators.Rbtree_freq.rbtree_gen inv color height

let completetree () =
  let size = 10 in
  Generators.Completetree_freq.complete_tree_gen size

let depthbst () =
  let depth = 5 in
  let low = 0 in
  let high = 100 in
  Generators.Depthbst_freq.size_bst_gen depth low high

let depthtree () =
  let depth = 5 in
  Generators.Depthtree_freq.depth_tree_gen depth

(* Dragen *)
let dragen_tree () =
  let size = 10 in
  Dragen.Tree.dragen_tree size

(* Loaded Dice *)
let ld_rbtree () =
  let size = 5 in
  LoadedDice.Rbtree.rbtree_ld_gen size BranchC

let sizedlist_para_enum () =
  let size = 5 in
  Parametrized_enumeration.Sizedlist_freq.sized_list_gen size

let sizedlist_para_1 () =
  let size = 10 in
  Parametrized.Sizedlist_freq_1.sized_list_gen size

let sizedlist_para_2 () =
  let size = 10 in
  Parametrized.Sizedlist_freq.sized_list_gen size

let evenlist_para_2 () =
  let size = 10 in
  Parametrized.Evenlist_freq.even_list_gen size

let depthtree_para_2 () =
  let depth = 5 in
  Parametrized.Depthtree_freq.depth_tree_gen depth

let depthbst_para_2 () =
  let depth = 5 in
  let low = 0 in
  let high = 100 in
  Parametrized.Depthbst_freq.size_bst_gen depth low high

let rbtree_para_2 () =
  let height = 2 in
  let color = true in
  let inv = if color then 2 * height else (2 * height) + 1 in
  Parametrized.Rbtree_freq.rbtree_gen inv color height

let depthtree_ur () =
  let depth = 5 in
  Unrolled.Depthtree_freq.depth_tree_gen depth

let depthbst_ur () =
  let depth = 5 in
  let low = 0 in
  let high = 100 in
  Unrolled.Depthbst_freq.size_bst_gen depth low high

let rbtree_ur () =
  let height = 2 in
  let color = true in
  let inv = if color then 2 * height else (2 * height) + 1 in
  Unrolled.Rbtree_freq.rbtree_gen inv color height

let sizedlist_ur () =
  let size = 10 in
  Unrolled.Sizedlist_freq.sized_list_gen size

let evenlist_ur () =
  let size = 10 in
  Unrolled.Evenlist_freq.even_list_gen size

let depthtree_5_ur () =
  let depth = 5 in
  Unrolled.Depthtree_5.depth_tree_gen depth

let depthbst_5_ur () =
  let depth = 5 in
  let low = 0 in
  let high = 100 in
  Unrolled.Depthbst_5.size_bst_gen depth low high

let sizedlist_5_ur () =
  let size = 10 in
  Unrolled.Sizedlist_5.sized_list_gen size

let sizedlist_10_ur () =
  let size = 10 in
  Unrolled.Sizedlist_10.sized_list_gen size

let depthtree_ur_lin () =
  let depth = 5 in
  Unrolled_linear.Depthtree_freq.depth_tree_gen depth

let depthbst_ur_lin () =
  let depth = 5 in
  let low = 0 in
  let high = 100 in
  Unrolled_linear.Depthbst_freq.size_bst_gen depth low high

(* rerolled functions *)
let sizedlist_rr () =
  let size = 20 in
  Rerolled.Sizedlist_freq.sized_list_gen size