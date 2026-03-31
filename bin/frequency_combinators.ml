(* should figure out the best location for this *)
(* let weights_f1 = ref (-400) *)
let weights = ref [| 100; 500 |]
let w_ = ref [| 0 |]

(** accesing the weight at index i*)
let get_weight_idx (i : int) = !weights.(i)

let set_w_idx (i : int) (x : int) = !weights.(i) <- x

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
  let base_case = (fst m_b * size) + c_b in
  let recursive_case = (fst m_rec * size) + c_rec in

  if base_case < 0 || recursive_case < 0 then
    raise (Neg_Weight (Printf.sprintf "%d %d" base_case recursive_case))
  else
    QCheck.Gen.frequency
      [ (base_case, snd m_b); (recursive_case, snd m_rec) ]
      (QCheck_runner.random_state ())

let freq_harmonic size b_thunk rec_thunk =
  let base_case = int_of_float (1. /. (float_of_int size +. 1.) *. 1000.) in
  let recursive_case =
    int_of_float ((1. -. (1. /. (float_of_int size +. 1.))) *. 1000.)
  in

  (* Printf.printf "%d %d %d\n" size base_case recursive_case; *)

  if base_case < 0 || recursive_case < 0 then
    raise (Neg_Weight (Printf.sprintf "%d %d" base_case recursive_case))
  else
    QCheck.Gen.frequency
      [ (base_case, b_thunk); (recursive_case, rec_thunk) ]
    (QCheck_runner.random_state ())

let find_p n =
  let p = Array.make n 0.0 in
  p.(n - 1) <- 1.0;
  p.(0) <- 1.0 /. Float.of_int n;
  for i = 1 to n - 2 do
    (* Peel off layers 0..i-1 from G(0,i) = (i+1)/n *)
    let target = ref (Float.of_int (i + 1) /. Float.of_int n) in
    for j = 0 to i - 1 do
      target := sqrt ((!target -. p.(j)) /. (1.0 -. p.(j)))
    done;
    p.(i) <- !target
  done;
  p

let freq_tree size b_thunk rec_thunk =
  (* need to remember total max bound *)
  let bound = 8 in
  (* let size = float_of_int size in
  let base_case = (1. /. ( size +. 1.)) ** (1. /. 2. ** (bound -. size)) in *)
  (* let base_case =
    match size with 3 -> 0.25 | 2 -> 1. /. (3. ** 0.5) | 1 -> 0.75 | _ -> 0.5
  in *)

  (* Printf.printf "finding p"; *)
  let p = find_p (bound + 1) in

  let print_int_list l =
  Array.iter (Printf.printf "%f ") l;
  print_newline () in

  (* Format.printf "@[<hov 0>Int array: %a@]@."
  (Format.pp_print_array Format.pp_print_float)
  p; *)

  print_int_list p;

  let base_case = p.(bound - size) in
  let recursive_case = 1. -. base_case in
  let base_case = int_of_float (base_case *. 1000.) in
  let recursive_case = int_of_float (recursive_case *. 1000.) in

  if base_case < 0 || recursive_case < 0 then
    raise (Neg_Weight (Printf.sprintf "%d %d" base_case recursive_case))
  else
    QCheck.Gen.frequency
      [ (base_case, b_thunk); (recursive_case, rec_thunk) ]
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
