open Combinators
open Frequency_combinators
(* from  Loaded Dice Paper (p.16) *)

(* type color_ld = Red | Black *)
(* type rbtree_ld = Leaf_ld | Branch_ld of rbtree_ld * int * color_ld * rbtree_ld *)
type color_c = RedC | BlackC
type rbtree_c = LeafC | BranchC

(* max height 5 *)
let rec rbtree_ld_gen size chosenCtr =
  match size with
  | 0 -> Rbtleaf
  | _ -> (
      match chosenCtr with
      | LeafC -> Rbtleaf
      | BranchC ->
          (* size : [1, 5] *)
          let w0, w1, w2, w3, w4, w5, w6, w7 =
            ( get_weight_idx ((size - 1) * 8),
              get_weight_idx (((size - 1) * 8) + 1),
              get_weight_idx (((size - 1) * 8) + 2),
              get_weight_idx (((size - 1) * 8) + 3),
              get_weight_idx (((size - 1) * 8) + 4),
              get_weight_idx (((size - 1) * 8) + 5),
              get_weight_idx (((size - 1) * 8) + 6),
              get_weight_idx (((size - 1) * 8) + 7) )
          in
          let b1, c, _, b2 =
            QCheck.Gen.frequency
              [
                (w0, fun _ -> (LeafC, RedC, (), LeafC));
                (w1, fun _ -> (LeafC, RedC, (), BranchC));
                (w2, fun _ -> (LeafC, BlackC, (), LeafC));
                (w3, fun _ -> (LeafC, BlackC, (), BranchC));
                (w4, fun _ -> (BranchC, RedC, (), LeafC));
                (w5, fun _ -> (BranchC, RedC, (), BranchC));
                (w6, fun _ -> (BranchC, BlackC, (), LeafC));
                (w7, fun _ -> (BranchC, BlackC, (), BranchC));
              ]
              (QCheck_runner.random_state ())
          in

          let branch1 = rbtree_ld_gen (size - 1) b1 in
          let color = if c = RedC then true else false in
          let x = int_gen () in
          let branch2 = rbtree_ld_gen (size - 1) b2 in

          Rbtnode (color, branch1, x, branch2))

let rec rbtree_ld_edit_gen size chosenCtr =
  match size with
  | 0 -> Rbtleaf
  | _ -> (
      match chosenCtr with
      | LeafC -> Rbtleaf
      | BranchC ->
          (* size : [1, 5] *)
          let w0, w1, w2, w3, w4, w5, w6, w7 =
            ( get_weight_idx ((size - 1) * 8),
              get_weight_idx (((size - 1) * 8) + 1),
              get_weight_idx (((size - 1) * 8) + 2),
              get_weight_idx (((size - 1) * 8) + 3),
              get_weight_idx (((size - 1) * 8) + 4),
              get_weight_idx (((size - 1) * 8) + 5),
              get_weight_idx (((size - 1) * 8) + 6),
              get_weight_idx (((size - 1) * 8) + 7) )
          in
          let b1, c, _, b2 =
            QCheck.Gen.frequency
              [
                (w0, fun _ -> (LeafC, RedC, (), LeafC));
                (w1, fun _ -> (LeafC, RedC, (), BranchC));
                (w2, fun _ -> (LeafC, BlackC, (), LeafC));
                (w3, fun _ -> (LeafC, BlackC, (), BranchC));
                (w4, fun _ -> (BranchC, RedC, (), LeafC));
                (w5, fun _ -> (BranchC, RedC, (), BranchC));
                (w6, fun _ -> (BranchC, BlackC, (), LeafC));
                (w7, fun _ -> (BranchC, BlackC, (), BranchC));
              ]
              (QCheck_runner.random_state ())
          in

          let branch1 = rbtree_ld_edit_gen (size - 1) b1 in
          let color = if c = RedC then true else false in
          let x = int_gen () in
          let branch2 = rbtree_ld_edit_gen (size - 1) b2 in

          Rbtnode (color, branch1, x, branch2))
