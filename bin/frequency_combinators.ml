(* should figure out the best location for this *)
(* let weights_f1 = ref (-400) *)
let weights = ref [| 100; 500 |]

(* for rbt, first 2 freq are for the leaf, and the last 2 are for the standard *)

(* binary search tree example from patrick *)
let frequency_gen_bst size ~base_case ~recursive_case =
  QCheck.Gen.frequency
    [ (1, base_case); (size, recursive_case) ]
    (QCheck_runner.random_state ())

(* basic example for sized list *)
let frequency_gen_list_basic base_case recursive_case =
  QCheck.Gen.frequency
    [ (1, base_case); (1, recursive_case) ]
    (QCheck_runner.random_state ())

let frequency_gen_list base_case recursive_case =
  QCheck.Gen.frequency
    [ (fst base_case, snd base_case); (fst recursive_case, snd recursive_case) ]
    (QCheck_runner.random_state ())

let frequency_gen_n base_case recursive_case =
  QCheck.Gen.frequency
    [ (0, snd base_case); (1, snd recursive_case) ]
    (QCheck_runner.random_state ())

(** uniform distribution *)
let unif_gen fst_case snd_case =
  QCheck.Gen.frequency
    [ (1, fst_case); (1, snd_case) ]
    (QCheck_runner.random_state ())

(** accesing the weight at index i*)
let get_weight_idx (i : int) = !weights.(i)
