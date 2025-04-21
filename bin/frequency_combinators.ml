
(* binary search tree example from patrick *)
let frequency_gen_bst size ~base_case ~recursive_case =
  QCheck.Gen.frequency
    [ (1, base_case); (size, recursive_case) ]
    (QCheck_runner.random_state ())

(* basic example for sized list *)