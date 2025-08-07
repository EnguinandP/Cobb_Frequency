open Frequency_combinators

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

(* list feature vectors *)

(** distrition of lists that are nil *)
let nil_fv acc x = if [] = x then acc +. 1. else acc

(** avg length of lists *)
let len_fv acc x =
  let s = List.length x in
  acc +. float_of_int s

(* rbtree feature vectors *)

let rec count_rb (tree : int Combinators.rbtree) (r, b) =
  match tree with
  | Rbtleaf -> (r, b)
  | Rbtnode (c, lt, _, rt) ->
      let ltr, ltb = count_rb lt (0, 0) in
      let rtr, rtb = count_rb rt (0, 0) in
      if c then (ltr + rtr + 1, ltb + rtb) else (ltr + rtr, ltb + rtb + 1)

(** feature vector is the percentage of black nodes *)
let b_fv acc x =
  let r, b = count_rb x (0, 0) in
  acc +. (float_of_int b /. float_of_int (r + b))
(* max r to b nodes is 2 : 1 (only possible for even h *)

(* let count_bailout n gen count =
  if n >= sample_size then
    count
  else
    let count = 
    try
      let _ = gen ();
      count
    with 
    | Combinators.BailOut -> count + 1
  in

let score_bailout gen (goal : float) =  *)

(** noise is normal/gaussian distribution using marsaglia-polar method *)
let calc_noisy_score spare results goal feature_vector =
  (* assumed standard deviation *)
  let std_dev = 3.0 in

  match spare with
  | None ->
      let rec find_t () =
        let u = Random.float 2. -. 1. in
        let v = Random.float 2. -. 1. in
        let t = (u *. u) +. (v *. v) in
        if t <= 1. && t > 0. then (u, v, t) else find_t ()
      in
      let u, v, t = find_t () in
      let s = Float.sqrt (-2. *. Float.log t /. t) in
      let spare = Some (s *. v) in
      let noise = results +. (std_dev *. u *. s) in
      (noise, spare, goal -. noise)
  | Some s ->
      let spare = None in
      let noise = results +. (std_dev *. s) in
      (noise, spare, goal -. noise)

(* chi squared test *)
let calc_uniform results goal size =
  let distr = Array.make 11 0 in
  List.iter
    (fun x ->
      let length = List.length x in
      distr.(length) <- distr.(length) + 1)
    results;
  let sum =
    List.fold_left (fun acc x -> acc +. float_of_int (List.length x)) 0. results
  in
  let avg = sum /. float_of_int (List.length results) in
  let chi_squared =
    List.fold_left
      (fun acc x ->
        let length = List.length x in
        let diff = float_of_int (length - int_of_float avg) in
        acc +. (diff *. diff /. avg))
      0. results
  in
  chi_squared

let is_uniform l =
  let rec aux acc = function [] -> acc | x :: xs -> aux (acc +. x) xs in
  let sum = aux 0. l in
  let avg = sum /. float_of_int (List.length l) in
  let diff = List.fold_left (fun acc x -> acc +. abs_float (x -. avg)) 0. l in
  diff

(* function examples *)
(* 1 paramater functions *)
let f1 () =
  let x = float_of_int !weights.(0) in
  x /. 10. *. cos (x /. 25.)

(* (547.047, -77.145) *)
let f2 () : float =
  let x = float_of_int !weights.(0) in
  (4. /. 30. *. (x +. 50.) *. sin ((x +. 50.) /. 20.))
  +. ((x -. 500.) /. 30. *. ((x -. 500.) /. 30.))

(* (-1, -987.688) *)
let f3 () =
  let x = float_of_int !weights.(0) in
  if x = 0. then 1. else cos (50. *. Float.pi *. x /. 1000.) /. (x /. 1000.)

(* (1, -315.552) *)
let f4 () =
  let x = float_of_int !weights.(0) in
  -500. *. (sin x *. 30.) /. (x *. 40.)

(* 2 parameter functions *)

(* cone *)
let f5 () =
  let x = float_of_int !weights.(0) in
  let y = float_of_int !weights.(1) in
  (x /. 30. *. (x /. 30.)) +. (y /. 30. *. (y /. 30.))

let f6 () =
  let x = float_of_int !weights.(0) in
  let y = float_of_int !weights.(1) in
  (-500. *. (sin x *. 30.) /. (x *. 40.))
  +. (80. *. sin (y /. 30.) /. (y /. 100.))

(* 3 parameter functions *)
let f7 () =
  let x = float_of_int !weights.(0) in
  let y = float_of_int !weights.(1) in
  let z = float_of_int !weights.(2) in

  (-500. *. (sin x *. 30.) /. (x *. 40.))
  +. (80. *. sin (y /. 30.) /. (y /. 100.))
  +. z
