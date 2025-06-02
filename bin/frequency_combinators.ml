(* should figure out the best location for this *)
let weights_f1 = ref (-400)
let weights1 = ref [|400|]
(* let weights = ref [|400; 400|] *)
let weights = ref [|400;|]

(* binary search tree example from patrick *)
let frequency_gen_bst size ~base_case ~recursive_case =
  QCheck.Gen.frequency
    [ (1, base_case); (size, recursive_case) ]
    (QCheck_runner.random_state ())

(* basic example for sized list *)
let frequency_gen_list_old base_case recursive_case =
  QCheck.Gen.frequency
  [(1, base_case); (1, recursive_case)]
  (QCheck_runner.random_state ())

let frequency_gen_list base_case recursive_case =
  QCheck.Gen.frequency
  [
    (fst base_case, (snd base_case)); 
    (fst recursive_case, (snd recursive_case))
    ]
  (QCheck_runner.random_state ())


  (* accesing the weights *)
  let get_weight_idx (i: int) = !weights.(i)
