open Frequency_combinators
open Combinators

let sample = ref 1000
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

(* minimizes dist *)
let get_score fv goal results : (float * float list) * float =
  let pass = List.fold_left fv 0. results in

  let dist = pass /. float_of_int (List.length results) in
  let goal =
    match goal with g :: [] -> g | _ -> failwith "goal format error"
  in
  let score = dist -. goal in

  ((dist, []), score)

(* minimizes dist *)
let get_score2 (fv : float -> 'a -> float) goal (results : 'a list) :
    (float * float list) * float =
  let x = List.map (fun x -> fv 0. x) results in
  let pass = List.fold_left fv 0. results in

  let n = float_of_int (List.length results) in

  let dist = pass /. n in
  let goal =
    match goal with g :: [] -> g | _ -> failwith "goal format error"
  in

  (* alpha = .05, one-sided *)
  let crit_val = -1.645 in
  let sum_dif_sqr =
    List.fold_left (fun x acc -> acc +. ((x -. dist) ** 2.)) 0. x
  in
  let std_dev = Float.sqrt (sum_dif_sqr /. (n -. 1.)) in
  let t = (dist -. goal) /. (std_dev /. Float.sqrt n) in

  let score = t -. crit_val in

  ((dist, []), score)

let get_exact_score2 fv goal results : (float * float list) * float =
  let x =
    List.map
      (fun x ->
        let a = fv 0. x in
        (* Printf.printf "%f\n" a; *)
        fv 0. x)
      results
  in
  let pass = List.fold_left fv 0. results in

  let n = float_of_int (List.length results) in

  let dist = pass /. n in
  let goal =
    match goal with g :: [] -> g | _ -> failwith "goal format error"
  in

  (* alpha = .05, two sided *)
  let crit_val = 1.962 in
  let sum_dif_sqr =
    List.fold_left
      (fun acc x ->
        (* Printf.printf "x= %f c=%f\n" x ((x -. dist) ** 2.); *)
        acc +. ((x -. dist) ** 2.))
      0. x
  in
  let std_dev = Float.sqrt (sum_dif_sqr /. (n -. 1.)) in
  let t = (dist -. goal) /. (std_dev /. Float.sqrt n) in
  (* Printf.printf "dist= %f s = %f %f\n" dist std_dev t; *)
  (* Printf.printf "sum= %f n= %f c=%f %f\n" sum_dif_sqr n ((x -. dist) ** 2.) std_dev; *)

  let score = Float.abs t -. crit_val in

  ((dist, []), score)

(* minimizes distance to dist *)
let get_exact_score fv goal results : (float * float list) * float =
  let pass = List.fold_left fv 0. results in

  let dist = pass /. float_of_int (List.length results) in
  let goal =
    match goal with g :: [] -> g | _ -> failwith "goal format error"
  in
  let score = Float.abs (dist -. goal) in

  ((dist, []), score)

(* alpha = .05 and (i + 1) degrees of freedom *)
let crit_vals =
  [|
    3.841;
    5.991;
    7.815;
    9.488;
    11.070;
    12.592;
    14.067;
    15.507;
    16.919;
    18.307;
    19.675;
    21.026;
  |]

(** chi-square goodness of fit

    when target is multiple values*)
let get_chi_score fv goal results : (float * float list) * float =
  let size = List.length goal in
  (* avg *)
  let obs = List.map fv results in
  let obs =
    List.fold_left
      (fun acc x -> List.map2 ( +. ) acc x)
      (List.init size (fun _ -> 0.))
      obs
  in
  let obs = List.map (fun x -> x /. float_of_int (List.length results)) obs in

  let chi, n_none =
    List.fold_left2
      (fun (acc, n) o e ->
        if e = -1. then (acc, n) else (((o -. e) *. (o -. e) /. e) +. acc, n + 1))
      (0., 0) obs goal
  in

  (* alpha = .05 *)
  let crit = crit_vals.(n_none - 1) in
  ((chi, obs), chi -. crit)

(* score where target is uniform dist *)

let get_uniform_score accumulator (buckets : float list) results =
  (* let size =
    match size with
    | s :: [] -> s
    | _ -> failwith "expected single element for size"
  in *)
  let size = float_of_int (List.length buckets) in

  (* buckets are each size *)
  let goal =
    List.init (int_of_float size) (fun _ -> float_of_int !sample /. size)
  in

  (* print_float (float_of_int 1000 /. size); *)
  let obs_arr =
    List.fold_left (accumulator buckets)
      (Array.init (int_of_float size) (fun _ -> 0.))
      results
  in
  let obs = Array.to_list obs_arr in

  (* List.iter (fun x -> Printf.printf "%.3f, " x) obs;
  print_newline ();
  print_newline (); *)
  (* List.iter (fun x -> Printf.printf "%.3f, " x) goal;
  print_newline (); *)
  let chi =
    List.fold_left2
      (fun acc o e ->
        if e = -1. then acc else ((o -. e) *. (o -. e) /. e) +. acc)
      0. obs goal
  in
  let crit = crit_vals.(int_of_float size) in

  (* Printf.printf "chi = %.3f %.3f %.3f \n" chi crit (chi -. crit);  *)
  ((chi, obs), chi -. crit)

(* uniform length *)

let length_acc buckets acc x =
  let length = List.length x in
  
  let i = List.find_index (fun x -> x = (float_of_int length)) buckets in
  let i = match i with 
  | Some i' -> i' 
  | None -> failwith "wrong buckets" in

  acc.(i) <- acc.(i) +. 1.;
  acc

(* 
let length_acc acc x =
  let length = List.length x in
  let l = Array.length acc in
  acc.(length) <- acc.(length) +. 1.;
  acc *)

let rec count_constr_list (tree : Frequency_combinators.dragen_tree) =
  match tree with
  | Frequency_combinators.LeafA -> [ 1.; 0.; 0.; 0. ]
  | Frequency_combinators.LeafB -> [ 0.; 1.; 0.; 0. ]
  | Frequency_combinators.LeafC -> [ 0.; 0.; 1.; 0. ]
  | Frequency_combinators.Node (lt, rt) ->
      let ltc = count_constr_list lt in
      let rtc = count_constr_list rt in
      List.map2 ( +. ) ltc
        (List.mapi (fun i x -> if i = 3 then x +. 1. else x) rtc)

(* list feature vectors *)

(** distrition of lists that are nil *)
let nil_fv acc x = if [] = x then acc +. 1. else acc

(** avg length of lists *)
let len_fv acc x =
  let s = List.length x in
  acc +. float_of_int s

(* tree feature vectors *)

(* avg height of trees *)
let rec get_height x =
  match x with
  | Leaf -> 0.
  | Node (_, lt, rt) -> 1. +. max (get_height lt) (get_height rt)

let height_fv acc x = acc +. get_height x

let height_tree_acc buckets acc x =
  let height = int_of_float (get_height x) in

  (* Printf.printf "%d\n" height; *)

  let i = List.find_index (fun x -> x = (float_of_int height)) buckets in
  let i = match i with 
  | Some i' -> i' 
  | None -> failwith "wrong buckets" in

  acc.(i) <- acc.(i) +. 1.;
  acc

(** avg difference between right subtree and left subtree

    tree is height balanced when each lt and rt height difference is <= 1 *)

let h_balanced_fv acc x =
  let rec aux acc' x' =
    match x' with
    | Leaf -> (0., acc')
    | Node (_, lt, rt) ->
        let lth, rtacc = aux acc' lt in
        let rth, ltacc = aux acc' rt in
        (1. +. max lth rth, acc' +. Float.abs (lth -. rth))
  in

  let h, h_bal = aux acc x in
  h_bal

let rec count_stick (s, ns) x =
  match x with
  | Leaf -> (s, ns)
  | Node (_, Leaf, rt) ->
      let s, ns = count_stick (s, ns) rt in
      (s +. 1., ns)
  | Node (_, lt, Leaf) ->
      let s, ns = count_stick (s, ns) lt in
      (s +. 1., ns)
  | Node (_, lt, rt) -> (s, ns +. 1.)

(** percentage of nodes that are sticks *)
let stick_fv acc x =
  let p =
    if x = Leaf then 0.
    else
      let s, ns = count_stick (0., 0.) x in
      s /. (ns +. s)
  in
  acc +. p

(* rbtree feature vectors *)

let rec valid_rbt inv color tree =
  match tree with
  | Rbtleaf -> (color, 1)
  | Rbtnode (c, rt, _, lt) ->
      let rc, rinv = valid_rbt inv color rt in
      let lc, linv = valid_rbt inv color lt in

      let color =
        rc && lc && rinv = linv
        && (match (c, rt) with
           | true, Rbtnode (true, _, _, _) -> false
           | _ -> true)
        &&
        match (c, lt) with true, Rbtnode (true, _, _, _) -> false | _ -> true
      in
      let inv = if color = false then inv + 1 else inv in
      (color, inv)

let rec get_height_rbt x =
  match x with
  | Rbtleaf -> 0.
  | Rbtnode (_, lt, _, rt) -> 1. +. max (get_height_rbt lt) (get_height_rbt rt)

let height_rbt_acc buckets acc x =
  let height = int_of_float (get_height_rbt x) in

  let i = List.find_index (fun x -> x = (float_of_int height)) buckets in
  let i = match i with 
  | Some i' -> i' 
  | None -> failwith "wrong buckets" in

  acc.(i) <- acc.(i) +. 1.;
  acc


let rec count_rb tree =
  match tree with
  | Rbtleaf -> (0, 0)
  | Rbtnode (c, lt, _, rt) ->
      let ltr, ltb = count_rb lt in
      let rtr, rtb = count_rb rt in
      if c then (ltr + rtr + 1, ltb + rtb) else (ltr + rtr, ltb + rtb + 1)

(** feature vector is the percentage of black nodes *)
let b_fv acc x =
  let r, b = count_rb x in
  acc +. (float_of_int b /. float_of_int (r + b))
(* max r to b nodes is 2 : 1 (only possible for even h *)

let bailout_fv acc x = match x with None -> acc +. 1. | Some _ -> acc

(* number of bailouts hit *)
(* let bailout_fv =

let count_bailout n gen count =
  if n >= sample then
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

(* assume acc [0,0,0,0,0,0,0,0,...] *)
(* 
expected is [1,1,1,1...] 
count how many in each bin
conduct chi -> score
*)

(** chi squared test *)
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
