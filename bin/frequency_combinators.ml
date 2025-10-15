(* should figure out the best location for this *)
(* let weights_f1 = ref (-400) *)
let weights = ref [| 100; 500 |]

let w_ = ref [| 0 |]

(** accesing the weight at index i*)
let get_weight_idx (i : int) = !weights.(i)

let set_w_idx (i : int) (x :int) = !weights.(i) <- x

type dragen_tree = LeafA | LeafB | LeafC | Node of dragen_tree * dragen_tree

exception Neg_Weight of string

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

let freq_para_enum_gen size base_case recursive_case =
  let weights =
    match size with
    | 0 -> (get_weight_idx 0, get_weight_idx 1)
    | 1 -> (get_weight_idx 2, get_weight_idx 3)
    | 2 -> (get_weight_idx 4, get_weight_idx 5)
    | 3 -> (get_weight_idx 6, get_weight_idx 7)
    | 4 -> (get_weight_idx 8, get_weight_idx 9)
    | 5 -> (get_weight_idx 10, get_weight_idx 11)
    | 6 -> (get_weight_idx 12, get_weight_idx 13)
    | 7 -> (get_weight_idx 14, get_weight_idx 15)
    | 8 -> (get_weight_idx 16, get_weight_idx 17)
    | 9 -> (get_weight_idx 18, get_weight_idx 19)
    | 10 -> (get_weight_idx 20, get_weight_idx 21)
    | _ -> (1, 1)
  in

  QCheck.Gen.frequency
    [ (fst weights, snd base_case); (snd weights, snd recursive_case) ]
    (QCheck_runner.random_state ())

let freq_para_1_gen size c base_case recursive_case =
  QCheck.Gen.frequency
    [
      ((fst base_case * size) + c, snd base_case);
      ((fst recursive_case * size) + c, snd recursive_case);
    ]
    (QCheck_runner.random_state ())

let freq_para_2_gen size c_b c_rec m_b m_rec =
  let base_case = ((fst m_b * size) + c_b) in
  let recursive_case = ((fst m_rec * size) + c_rec) in
  
  if base_case < 0 || recursive_case < 0 then
    raise (Neg_Weight (Printf.sprintf "%d %d" base_case recursive_case))
  else

  QCheck.Gen.frequency
    [
      (base_case, snd m_b);
      (recursive_case, snd m_rec);
    ]
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

let nat_freq_gen w0 w1 w2 w3 w4 =
  QCheck.Gen.frequency
    [
      (w0, fun _ -> Random.State.int (QCheck_runner.random_state ()) 10);
      (* 0 - 9 *)
      (w1, fun _ -> Random.State.int (QCheck_runner.random_state ()) 100 + 10);
      (* 10 - 99 *)
      (w2, fun _ -> Random.State.int (QCheck_runner.random_state ()) 1000 + 100);
      (* 100 - 999 *)
      ( w3,
        fun _ -> Random.State.int (QCheck_runner.random_state ()) 10000 + 1000
      );
      (* 1000 - 9999 *)
      ( w4,
        fun _ -> Random.State.int (QCheck_runner.random_state ()) 100000 + 10000
      );
      (* 10000 - 99999 *)
    ]
    (QCheck_runner.random_state ())

let nat_freq_size_gen size w0 w1 w2 =
  QCheck.Gen.frequency
    [
      (w0, fun _ -> size);
      (* size *)
      (w1, fun _ -> Random.State.int (QCheck_runner.random_state ()) size);
      (* x < size *)
      (w2, fun _ -> Random.State.int (QCheck_runner.random_state ()) 1000 + size);
      (* size < x 1000 *)
    ]
    (QCheck_runner.random_state ())

let nat_freq_x_gen x w0 w1 w2 =
  QCheck.Gen.frequency
    [
      (w0, fun _ -> x);
      (* size *)
      (w1, fun _ -> Random.State.int (QCheck_runner.random_state ()) x);
      (* x < size *)
      (w2, fun _ -> Random.State.int (QCheck_runner.random_state ()) 1000 + x);
      (* size < x 1000 *)
    ]
    (QCheck_runner.random_state ())
