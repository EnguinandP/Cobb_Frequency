open Combinators
open Frequency_combinators

(* Dragen case study *)
type dragen_tree = LeafA | LeafB | LeafC | Node of dragen_tree * dragen_tree

(** generator for tree used in Dragen *)
let rec dragen_tree s =
  if s <= 0 then
    let w0 = get_weight_idx 0 in
    let w1 = get_weight_idx 1 in
    let w2 = get_weight_idx 2 in
    QCheck.Gen.frequency
      [ (w0, fun _ -> LeafA); (w1, fun _ -> LeafB); (w2, fun _ -> LeafC) ]
      (QCheck_runner.random_state ())
  else
    let w0 = get_weight_idx 0 in
    let w1 = get_weight_idx 1 in
    let w2 = get_weight_idx 2 in
    let w3 = get_weight_idx 3 in
    let w4 = get_weight_idx 4 in
    let w5 = get_weight_idx 5 in
    frequency_gen_list
      ( w4,
        fun _ ->
          QCheck.Gen.frequency
            [ (w1, fun _ -> LeafA); (w2, fun _ -> LeafB); (w3, fun _ -> LeafC) ]
            (QCheck_runner.random_state ()) )
      ( w5,
        fun _ ->
          let l = dragen_tree (s - 1) in
          let r = dragen_tree (s - 1) in
          Node (l, r) )
