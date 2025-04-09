open Combinators

(* dealing with exception Bailout *)
let catch_err f = fun () -> try Some (f ()) with BailOut -> None

(* Cobb generators *)

(* sized lists *)
let sized_list_generators =
  [
    (Sized_list.Prog.sized_list_gen, "prog");
    ((fun _ -> int_list_gen ()), "default");
    (Sized_list.Prog1_syn.sized_list_gen, "prog1_syn");
    (Sized_list.Prog2_syn.sized_list_gen, "prog2_syn");
    (Sized_list.Prog3_syn.sized_list_gen, "prog3_syn");
    (Sized_list.Prog4_syn.sized_list_gen, "prog4_syn");
    (Sized_list.Prog5_syn.sized_list_gen, "prog5_syn");
    (Sized_list.Prog6_syn.sized_list_gen, "prog6_syn");
    (Sized_list.Prog7_syn.sized_list_gen, "prog7_syn");
    (Sized_list.Prog8_syn.sized_list_gen, "prog8_syn");
    (Sized_list.Prog9_syn.sized_list_gen, "sketch_syn");
    (Sized_list.Prog1_cov.sized_list_gen, "prog1_cov");
    (Sized_list.Prog2_cov.sized_list_gen, "prog2_cov");
    (Sized_list.Prog3_cov.sized_list_gen, "prog3_cov");
    (Sized_list.Prog4_cov.sized_list_gen, "prog4_cov");
    (Sized_list.Prog5_cov.sized_list_gen, "prog5_cov");
    (Sized_list.Prog6_cov.sized_list_gen, "prog6_cov");
    (Sized_list.Prog7_cov.sized_list_gen, "prog7_cov");
    (Sized_list.Prog8_cov.sized_list_gen, "prog8_cov");
    (Sized_list.Prog9_cov.sized_list_gen, "sketch_cov");
    (Sized_list.Prog1_safe.sized_list_gen, "prog1_safe");
    (Sized_list.Prog2_safe.sized_list_gen, "prog2_safe");
    (Sized_list.Prog3_safe.sized_list_gen, "prog3_safe");
    (Sized_list.Prog4_safe.sized_list_gen, "prog4_safe");
    (Sized_list.Prog5_safe.sized_list_gen, "prog5_safe");
    (Sized_list.Prog6_safe.sized_list_gen, "prog6_safe");
    (Sized_list.Prog7_safe.sized_list_gen, "prog7_safe");
    (Sized_list.Prog8_safe.sized_list_gen, "prog8_safe");
    (Sized_list.Prog9_safe.sized_list_gen, "sketch_safe");
  ]

(* duplicate lists *)
let duplicate_list_generators =
  [
    (Duplicate_list.Prog.duplicate_list_gen, "prog");
    ((fun _ _ -> int_list_gen ()), "default");
    (Duplicate_list.Prog1_syn.duplicate_list_gen, "prog1_syn");
    (Duplicate_list.Prog2_syn.duplicate_list_gen, "prog2_syn");
    (Duplicate_list.Prog3_syn.duplicate_list_gen, "sketch_syn");
    (Duplicate_list.Prog1_cov.duplicate_list_gen, "prog1_cov");
    (Duplicate_list.Prog2_cov.duplicate_list_gen, "prog2_cov");
    (Duplicate_list.Prog3_cov.duplicate_list_gen, "sketch_cov");
    (Duplicate_list.Prog1_safe.duplicate_list_gen, "prog1_safe");
    (Duplicate_list.Prog2_safe.duplicate_list_gen, "prog2_safe");
    (Duplicate_list.Prog3_safe.duplicate_list_gen, "sketch_safe");
  ]

(* Unique lists *)
let unique_list_generators =
  [
    (Unique_list.Prog.unique_list_gen, "prog");
    ((fun _ -> int_list_gen ()), "default");
    (Unique_list.Prog1_syn.unique_list_gen, "prog1_syn");
    (Unique_list.Prog2_syn.unique_list_gen, "prog2_syn");
    (Unique_list.Prog3_syn.unique_list_gen, "sketch_syn");
    (Unique_list.Prog1_cov.unique_list_gen, "prog1_cov");
    (Unique_list.Prog2_cov.unique_list_gen, "prog2_cov");
    (Unique_list.Prog3_cov.unique_list_gen, "sketch_cov");
    (Unique_list.Prog1_safe.unique_list_gen, "prog1_safe");
    (Unique_list.Prog2_safe.unique_list_gen, "prog2_safe");
    (Unique_list.Prog3_safe.unique_list_gen, "sketch_safe");
  ]

(* sorted lists *)
let sorted_list_generators =
  [
    (Sorted_list.Prog.sorted_list_gen, "prog");
    ((fun _ _ -> int_list_gen ()), "default");
    (Sorted_list.Prog1_syn.sorted_list_gen, "prog1_syn");
    (Sorted_list.Prog2_syn.sorted_list_gen, "prog2_syn");
    (Sorted_list.Prog3_syn.sorted_list_gen, "sketch_syn");
    (Sorted_list.Prog1_cov.sorted_list_gen, "prog1_cov");
    (Sorted_list.Prog2_cov.sorted_list_gen, "prog2_cov");
    (Sorted_list.Prog3_cov.sorted_list_gen, "sketch_cov");
    (Sorted_list.Prog1_safe.sorted_list_gen, "prog1_safe");
    (Sorted_list.Prog2_safe.sorted_list_gen, "prog2_safe");
    (Sorted_list.Prog3_safe.sorted_list_gen, "sketch_safe");
  ]

(* TODO: add a sketch *)
let rbtree_generators =
  [
    (Rbtree.Prog.rbtree_gen, "prog");
    ((fun _ _ _ -> default_rbtree_gen ()), "default");
    (Rbtree.Prog1_syn.rbtree_gen, "prog1_syn");
    (Rbtree.Prog2_syn.rbtree_gen, "prog2_syn");
    (Rbtree.Prog3_syn.rbtree_gen, "prog3_syn");
    (Rbtree.Prog4_syn.rbtree_gen, "prog4_syn");
    (Rbtree.Prog5_syn.rbtree_gen, "prog5_syn");
    (Rbtree.Prog6_syn.rbtree_gen, "prog6_syn");
    (Rbtree.Prog7_syn.rbtree_gen, "sketch_syn");
    (Rbtree.Prog1_cov.rbtree_gen, "prog1_cov");
    (Rbtree.Prog2_cov.rbtree_gen, "prog2_cov");
    (Rbtree.Prog3_cov.rbtree_gen, "prog3_cov");
    (Rbtree.Prog4_cov.rbtree_gen, "prog4_cov");
    (Rbtree.Prog5_cov.rbtree_gen, "prog5_cov");
    (Rbtree.Prog6_cov.rbtree_gen, "prog6_cov");
    (Rbtree.Prog7_cov.rbtree_gen, "sketch_cov");
    (Rbtree.Prog1_safe.rbtree_gen, "prog1_safe");
    (Rbtree.Prog2_safe.rbtree_gen, "prog2_safe");
    (Rbtree.Prog3_safe.rbtree_gen, "prog3_safe");
    (Rbtree.Prog4_safe.rbtree_gen, "prog4_safe");
    (Rbtree.Prog5_safe.rbtree_gen, "prog5_safe");
    (Rbtree.Prog6_safe.rbtree_gen, "prog6_safe");
    (Rbtree.Prog7_safe.rbtree_gen, "sketch_safe");
  ]

let complete_tree_generators =
  [
    (Complete_tree.Prog.complete_tree_gen, "prog");
    ((fun _ -> tree_gen ()), "default");
    (Complete_tree.Prog1_syn.complete_tree_gen, "prog1_syn");
    (Complete_tree.Prog2_syn.complete_tree_gen, "prog2_syn");
    (Complete_tree.Prog3_syn.complete_tree_gen, "sketch_syn");
    (Complete_tree.Prog1_cov.complete_tree_gen, "prog1_cov");
    (Complete_tree.Prog2_cov.complete_tree_gen, "prog2_cov");
    (Complete_tree.Prog3_cov.complete_tree_gen, "sketch_cov");
    (Complete_tree.Prog1_safe.complete_tree_gen, "prog1_safe");
    (Complete_tree.Prog2_safe.complete_tree_gen, "prog2_safe");
    (Complete_tree.Prog3_safe.complete_tree_gen, "sketch_safe");
  ]

let depth_tree_gen =
  [
    (Depth_tree.Prog.depth_tree_gen, "prog");
    ((fun _ -> tree_gen ()), "default");
    (Depth_tree.Prog1_syn.depth_tree_gen, "prog1_syn");
    (Depth_tree.Prog2_syn.depth_tree_gen, "prog2_syn");
    (Depth_tree.Prog3_syn.depth_tree_gen, "prog3_syn");
    (Depth_tree.Prog4_syn.depth_tree_gen, "sketch_syn");
    (Depth_tree.Prog1_cov.depth_tree_gen, "prog1_cov");
    (Depth_tree.Prog2_cov.depth_tree_gen, "prog2_cov");
    (Depth_tree.Prog3_cov.depth_tree_gen, "prog3_cov");
    (Depth_tree.Prog4_cov.depth_tree_gen, "sketch_cov");
    (Depth_tree.Prog1_safe.depth_tree_gen, "prog1_safe");
    (Depth_tree.Prog2_safe.depth_tree_gen, "prog2_safe");
    (Depth_tree.Prog3_safe.depth_tree_gen, "prog3_safe");
    (Depth_tree.Prog4_safe.depth_tree_gen, "sketch_safe");
  ]

let depth_bst_gen =
  [
    (Depth_bst_tree.Prog.size_bst_gen, "prog");
    ((fun _ _ _ -> tree_gen ()), "default");
    (Depth_bst_tree.Prog1_syn.size_bst_gen, "prog1_syn");
    (Depth_bst_tree.Prog2_syn.size_bst_gen, "prog2_syn");
    (Depth_bst_tree.Prog3_syn.size_bst_gen, "prog3_syn");
    (Depth_bst_tree.Prog4_syn.size_bst_gen, "prog4_syn");
    (Depth_bst_tree.Prog5_syn.size_bst_gen, "sketch_syn");
    (Depth_bst_tree.Prog1_cov.size_bst_gen, "prog1_cov");
    (Depth_bst_tree.Prog2_cov.size_bst_gen, "prog2_cov");
    (Depth_bst_tree.Prog3_cov.size_bst_gen, "prog3_cov");
    (Depth_bst_tree.Prog4_cov.size_bst_gen, "prog4_cov");
    (Depth_bst_tree.Prog5_cov.size_bst_gen, "sketch_cov");
    (Depth_bst_tree.Prog1_safe.size_bst_gen, "prog1_safe");
    (Depth_bst_tree.Prog2_safe.size_bst_gen, "prog2_safe");
    (Depth_bst_tree.Prog3_safe.size_bst_gen, "prog3_safe");
    (Depth_bst_tree.Prog4_safe.size_bst_gen, "prog4_safe");
    (Depth_bst_tree.Prog5_safe.size_bst_gen, "sketch_safe");
  ]

let even_list_gen =
  [
    (Even_list.Prog.even_list_gen, "prog");
    ((fun _ -> int_list_gen ()), "default");
    (Even_list.Prog1_syn.even_list_gen, "prog1_syn");
    (Even_list.Prog2_syn.even_list_gen, "prog2_syn");
    (Even_list.Prog3_syn.even_list_gen, "prog3_syn");
    (Even_list.Prog4_syn.even_list_gen, "prog4_syn");
    (Even_list.Prog5_syn.even_list_gen, "prog5_syn");
    (Even_list.Prog6_syn.even_list_gen, "prog6_syn");
    (Even_list.Prog7_syn.even_list_gen, "sketch_syn");
    (Even_list.Prog1_cov.even_list_gen, "prog1_cov");
    (Even_list.Prog2_cov.even_list_gen, "prog2_cov");
    (Even_list.Prog3_cov.even_list_gen, "prog3_cov");
    (Even_list.Prog4_cov.even_list_gen, "prog4_cov");
    (Even_list.Prog5_cov.even_list_gen, "prog5_cov");
    (Even_list.Prog6_cov.even_list_gen, "prog6_cov");
    (Even_list.Prog7_cov.even_list_gen, "sketch_cov");
    (Even_list.Prog1_safe.even_list_gen, "prog1_safe");
    (Even_list.Prog2_safe.even_list_gen, "prog2_safe");
    (Even_list.Prog3_safe.even_list_gen, "prog3_safe");
    (Even_list.Prog4_safe.even_list_gen, "prog4_safe");
    (Even_list.Prog5_safe.even_list_gen, "prog5_safe");
    (Even_list.Prog6_safe.even_list_gen, "prog6_safe");
    (Even_list.Prog7_safe.even_list_gen, "sketch_safe");
  ]

(* arbitary values for generator *)
(* let sized_list_arbitraries = List.map (fun (gen, name) -> (fun x -> arb_builder' (gen x)), name) sized_list_generators *)
let sized_list_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = nat_gen () in
          (size, gen size)),
        name ))
    sized_list_generators

let duplicate_list_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = nat_gen () in
          let x = int_gen () in
          (size, x, gen size x)),
        name ))
    duplicate_list_generators

let unique_list_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = nat_gen () in
          (size, gen size)),
        name ))
    unique_list_generators

let even_list_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = nat_gen () in
          (size, gen size)),
        name ))
    even_list_gen

let sorted_list_arbitraries =
  List.map
    (fun (gen, name) ->
      ( catch_err (fun () ->
            let size = nat_gen () in
            let x = int_gen () in
            (size, x, gen size x)),
        name ))
    sorted_list_generators

let complete_tree_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = tree_height_gen () in
          (size, gen size)),
        name ))
    complete_tree_generators

let depth_tree_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = tree_height_gen () in
          (size, gen size)),
        name ))
    depth_tree_gen

let depth_bst_tree_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = tree_height_gen () in
          let lo = int_gen () in
          let hi = gt_int_gen lo in
          (size, lo, hi, gen size lo hi)),
        name ))
    depth_bst_gen

let rbtree_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let height = black_height_gen () in
          let color = bool_gen () in
          let inv = if color then 2 * height else (2 * height) + 1 in

          (inv, color, height, gen inv color height)),
        name ))
    rbtree_generators
