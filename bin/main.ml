module Env = Zzenv
open Frequency_combinators
open Stdlib

let init_temp = 300. (* 300 *) 
(* scipy default temperature is 5230 *)
let sample_size = 1000
let step_size = 1
let step_range = (1, 20)
let reset = 300

(* observation: simulated annealing will get stuck in a local min when 
- temp is low 
- the stepping range is not large enough to jump past and down far enough the next hill 
-> far enough is when the score is lower and current local min *)

(* more bumpy functions *)
(* 1-10 10-100 100-5000 *)
(* uniform dist *)
(* shapes of trees - nice distribution
balanceness - duplicate elements - input to sizedlist bounds - parent -c shild difference can't control
could control values at nodes different nat gens - number of elements/nodes in list/tree 
- pressures precondition to not fail
- reduce the error-ing out 
- number of red and black nodes 
- think html nodes ... 
- min duplicates

tyche making sense of Property-Base Testing Effective *)

(* hyperparameter *)
(* add best candidate + best score *)
(* pool of cand , test each one and take next step*)
(* list of best and final evaluation *)
(* reorder loop to propose eval decide *)
(* take test gen out and use functions with weird local min and max *)
(* branching process for quickcheck genrators Augustin Mista *)

(* bump iterations pretty high - 100,000 - 60,000 *)
(* 2. random restart *)
(* go to new start with init temp *)
(* 1. basin hopping *)
(* * cost function with noise *)
(* more complicated examples *)
(* traveling salesman problem / 8 queens *)
(* 3. 3 parameter function *)
(* 4. direction bias working *)
(* 3.5 step function works for arbitrary number of parameters *)

(* for restart, checkc if stuck in neighborhood *)

let rec collect n results gen =
  (* if !time_out_ref then raise Timed_out; *)
  if n = sample_size then results
  else
    let gen_value = gen () in
    (* let oc = open_out output in
    List.iter (Printf.fprintf oc "%d, ") gen_value;
    Printf.fprintf oc "\n"; 
    close_out oc;
    *)
  
    (* collects generated values in results *)
    let results = gen_value :: results in
    collect (n + 1) results gen

let exit_cond dist goal = dist <= goal

(* (dist, score) feature vector is is nl, score is difference between distr and 10% *)
let is_nil l = l = []
let score_nil results goal = 
  let pass = List.fold_left (fun acc x -> if is_nil x then acc +. 1. else acc) 0. results in
  let dist = pass /. float_of_int (List.length results) in
  (dist, dist -. goal)

(* feature vector is the percentage of black nodes 
score is the difference of avg from .5 
widest ratio of red to black internal nodes is 2 : 1 (only possible for even h)*)
let rec count_rb (tree : int Combinators.rbtree ) (r , b) = 
  match tree with
  | Rbtleaf -> (r, b)
  | Rbtnode (c, lt, _, rt) -> 
    let (ltr, ltb) = count_rb lt (0,0) in
    let (rtr, rtb) = count_rb rt (0,0) in
    if c then 
    (ltr + rtr + 1, ltb + rtb) else (ltr + rtr, ltb + rtb + 1)
    
let score_rbtree_black (results : int Combinators.rbtree list) (goal : float) =
  let pass = List.fold_left (fun acc x -> 
    let (r, b) = count_rb x (0, 0) in
    acc +. (float_of_int(b) /. float_of_int(r + b))
    ) 0. results
  in
  let dist = pass /. float_of_int (List.length results) in
  (dist, dist -. goal)
  

(* noise is normal/gaussian distribution using marsaglia-polar method *)
let calc_noisy_score spare results goal feature_vector =
  (* assumed standard deviation *)
  let std_dev = 3.0 in

  match spare with
  | None ->
    let rec find_t () =
      let u = (Random.float 2.) -. 1. in
      let v = (Random.float 2.) -. 1. in
      let t = u *. u +. v *. v in
      if t <= 1. && t > 0. then
        (u, v, t)
      else
        find_t ()
      in
    let (u, v, t) = find_t () in
    let s = Float.sqrt ((-2. *. Float.log (t)) /. t) in
    let spare = Some (s *. v) in
    let noise = results +. std_dev *. u *. s in
    (noise, spare, goal -. noise)

  | Some s ->
    let spare = None in
    let noise = results +. std_dev *. s in
    (noise, spare, goal -. noise)


(* chi squared test *)
let calc_uniform results goal size =
  let distr = Array.make 11 0 in
  List.iter (fun x ->
    let length = List.length x in
    distr.(length) <- distr.(length) + 1
  ) results;
  let sum = List.fold_left (fun acc x -> acc +. float_of_int (List.length x)) 0. results in
  let avg = sum /. float_of_int (List.length results) in
  let chi_squared = List.fold_left (fun acc x ->
    let length = List.length x in
    let diff = float_of_int (length - int_of_float avg) in
    acc +. ((diff *. diff) /. avg)
  ) 0. results in
  chi_squared
  
let is_uniform l =
  let rec aux acc = function
    | [] -> acc
    | x::xs -> aux (acc +. x) xs
  in
  let sum = aux 0. l in
  let avg = sum /. float_of_int (List.length l) in
  let diff = List.fold_left (fun acc x -> acc +. abs_float (x -. avg)) 0. l in
  diff


(* weights for the generator *)

(* let step_direction cand_weight direction =
  if direction && Random.bool () then
    weights_f1 := cand_weight + step_size
  else
    weights_f1 := cand_weight - step_size;
  !weights_f1 *)

let step_in_range cand_weight step_range =
  let step = Random.int (snd step_range) + fst step_range in
  let n = Array.length cand_weight in
  let direction = Random.int n in
  
  if Random.bool () then 
    !weights.(direction) <- cand_weight.(direction) + step
  else
     if (step >= cand_weight.(0)) then
      ()
    else
      !weights.(direction) <- cand_weight.(direction) - step;
  !weights

let step cand_weight =
  let step = Random.int (snd step_range) + fst step_range in
  if Random.bool () then
    !weights.(0) <- cand_weight.(0) + step
  else
    if (step >= cand_weight.(0)) then
      ()
    else
      !weights.(0) <- cand_weight.(0) - step;
  !weights

let step_n_param (cand_weight : int array) =
  let step = Random.int (snd step_range) + fst step_range in

  let n = Array.length cand_weight in
  let direction = Random.int n in

  (* this is much harder to read *)
  let s =
    if Random.bool () then
      step
    else
      (* weight can't be negative *)
      if (step >= cand_weight.(direction)) then
        0
      else
        step * -1
      in
  !weights.(direction) <- cand_weight.(direction) + s;
  weights
  

(* updates the temperature *)
let update_temp n = init_temp /. (float_of_int n +. 1.)

(* print function for results *)
let print_iterations oc n curr_weight cand_score dist time =
  Printf.fprintf oc "%s," n;
  Array.iter (fun x -> Printf.fprintf oc "%d," x) curr_weight;
  Array.iter (fun x -> Printf.fprintf oc "%d," x) !weights;
  let _ = Printf.fprintf oc "%f,%f,%f\n" cand_score dist time
  in ()

let print_solutions oc best_weights best_score = 
  match oc with
  | None -> ()
  | Some c ->
  print_iterations c "solution" best_weights best_score 0. 0.

let print_labels oc =
  match oc with
  | None -> ()
  | Some c ->
  Printf.fprintf c "iteration,";
  Array.iteri (fun i x -> Printf.fprintf c "curr weight %d," (i + 1)) !weights;
  Array.iteri (fun i x -> Printf.fprintf c "cand weight %d," (i + 1)) !weights;
  let _ = Printf.fprintf c "score,distribution,time\n"
  in ()

let time_out_ref = ref false

(* exception Timed_out *)

let () =
  Core.Signal.Expert.handle Core.Signal.alrm (fun (_ : Core.Signal.t) ->
      Printf.printf "Timed out";
      time_out_ref := true)

let simulated_annealing (result_oc: out_channel) (gen : unit -> 'a) (calc_score : 'a list -> float -> float * float) (goal : float)
      (niter : int) print_all =

  let temp = init_temp in

  let extra_oc = if print_all then Some result_oc else None in
  print_labels extra_oc;

  let rec loop n temp direction curr_weight curr_score best_weight best_score best_dist count spare =
      if n = niter || exit_cond best_dist goal then
        (best_weight, best_score, best_dist)
      else 

      let _ = step_n_param curr_weight in

      let temp = update_temp n in
    
      (* collecting results *)
      let start_time = Unix.gettimeofday () in
      let results = collect 0 [] gen in
      let end_time : float = Unix.gettimeofday () in

      (* calculates score *)
      let dist, cand_score = calc_score results goal in
      (* calculates score with noise *)
      (* let cand_score, spare, dist = calc_noisy_score spare results goal feature_vector in *)

      let _ = print_iterations result_oc (string_of_int n) curr_weight cand_score dist (end_time -. start_time) in

      (* when score is worse, e^+ -> true *)
      if Random.float 1.0 < Float.exp (-. (cand_score -. curr_score) /. temp) then
        (* keep weight just tested *)
        (* let _ = Printf.fprintf result_oc "ACCEPT " in *)
        let w = Array.map (fun r -> r) !weights in
        if (cand_score < best_score) then
          loop (n + 1) temp direction w cand_score w cand_score dist count spare
        else
          loop (n + 1) temp direction w cand_score best_weight best_score best_dist count spare
      else
        (* let _ = Printf.fprintf result_oc "REJECT " in *)
        loop (n + 1) temp (not direction) curr_weight curr_score best_weight best_score best_dist (count + 1) spare
  in

  let (best_weight, best_score, best_dist) = loop 0 temp true !weights 1. !weights (1000.) 1. 0 None in
  let _ = print_solutions extra_oc best_weight best_score in

  (best_weight, best_score, best_dist)


let basin_hoppping (result_oc: out_channel) (gen) calc_score (niter : int) goal print_all =
  (* initial solution is stored in the ref *)

  let extra_oc = if print_all then Some result_oc else None in
  print_labels extra_oc;

  (* minimize is local min search *)
  let rec minimize n best_weight best_score best_dist spare = (

    (* next step *)
    let _ = step_n_param best_weight in
    let results = collect 0 [] gen in
    let dist, next_score = calc_score results goal in
    (* let results = gen () in
    let next_score, spare, dist = calc_noisy_score spare results feature_vector in *)

    if n > 100 then 
      (best_weight, best_score, best_dist)
    else
      (* if score is closer to min, then take step *)
      let w = Array.map (fun r -> r) !weights in
      if next_score < best_score then
        minimize (n + 1) w next_score dist spare
      else
        minimize (n + 1) best_weight best_score best_dist spare
  ) in

  let rec loop curr_weight curr_min best_weight best_score best_dist n spare =
    if n > niter || exit_cond best_dist goal then
      (best_weight, best_score, best_dist)
    else 
      let temp = update_temp n in

      (* perturbation (step) *)
      let _ = step_in_range curr_weight (30,50) in  (* try adaptive stepwise *)

      (* let start_time = Unix.gettimeofday () in
      (* let results = gen () in *)
      let results = collect 0 [] gen in
      let end_time : float = Unix.gettimeofday () in
      let _, _ = calc_score results feature_vector in *)
      (* let _, spare, dist = calc_noisy_score spare results goal feature_vector in *)

      (* minimize *)
      let next_min_weight, next_min, next_dist = minimize 0 (Array.map (fun r -> r) !weights) 100000. 1. spare in
      (* Printf.printf "%d %f %f\n" !weights1.(0) next_score next_min; *)

      let _ = print_iterations result_oc (string_of_int n) next_min_weight next_min next_dist 0. in
      (* Printf.printf "%d,%d,%d,%f,%f,0,0\n" n curr_weight.(0) !weights.(0) next_min next_dist; *)

      (* acceptance test *)
      if Random.float 1.0 < Float.exp (-. (next_min -. curr_min) /. temp) then
        (* let w = Array.map (fun r -> r) !weights in *)
        if (next_min < best_score) then
          loop next_min_weight next_min next_min_weight next_min next_dist (n + 1) spare
        else 
          loop next_min_weight next_min best_weight best_score best_dist (n + 1) spare
      else
        loop curr_weight curr_min best_weight best_score best_dist (n + 1) spare
    in

  (* min is intial solution *)
  let init_weight, init_min, init_dist = minimize 0 (Array.map (fun r -> r) !weights) 100000. 1. None in
  let best_weights, best_score, best_dist = loop init_weight init_min init_weight init_min 1. 0 None in
  let _ = print_solutions extra_oc best_weights best_score in

  (best_weights, best_score, best_dist)

let random_restart (result_oc: out_channel) (gen : unit -> 'a) calc_score (goal : float) (niter : int) algor =
  let start_time = Unix.gettimeofday () in

  let restart_interval = niter / 5 in

  (* let result_oc = open_out output in *)
  print_labels (Some result_oc);

  let rec restart n best_weight best_score best_dist =
    (* restarts 5 times *)
    if n >= 5 then
      (best_weight, best_score, best_dist)
    else
        (* new location between -1000 and 1000 *)
        (* new location between 0 and 1000 *)
        let new_start = Array.map (fun _ -> Random.int 1000) !weights in
        weights := new_start;
        (* Printf.printf "\n%d\n" !weights.(0); *)
        let (weight, score, dist) = algor result_oc gen calc_score goal restart_interval false in

      if score < best_score then
        restart (n + 1) weight score dist
      else
        restart (n + 1) best_weight best_score best_dist

      in

  let (best_weight, best_score, best_dist) = restart 0 !weights 100000. 1. in
  let end_time = Unix.gettimeofday () in
  let _ = print_iterations result_oc "solution" best_weight best_score 0. 0. in
  (* close_out result_oc; *)
  (best_weight, best_score, best_dist, (end_time -. start_time))

let () = QCheck_runner.set_seed 42

(* function examples *)
(* 1 paramater functions *)
let f1 () = 
  let x = float_of_int(!weights.(0)) in
    ( x /. 10.) *. (cos (x /. 25.))

(* (547.047, -77.145) *)
let f2 () : float = 
  let x = float_of_int(!weights.(0)) in
  (4. /. 30.) *. ( (x +. 50.)) *. sin ((x +. 50.) /. 20.) +. ((x -. 500.) /. 30.) *. ((x -. 500.) /. 30.)

(* (-1, -987.688) *)
let f3 () = 
  let x = float_of_int(!weights.(0)) in
  if x = 0. then
    1. 
  else
    cos (50. *. Float.pi *. x /. 1000.) /. (x /. 1000.)

(* (1, -315.552) *)
let f4 () = 
  let x = float_of_int(!weights.(0)) in
  -500. *. (sin x *. 30.) /. (x *. 40.)

(* 2 parameter functions *)

(* cone *)
let f5 () = 
  let x = float_of_int(!weights.(0)) in
  let y = float_of_int(!weights.(1)) in
  (x /. 30.) *. (x /. 30.) +. (y /. 30.) *. (y /. 30.)

let f6 () = 
  let x = float_of_int(!weights.(0)) in
  let y = float_of_int(!weights.(1)) in
  -500. *. (sin x *. 30.) /. (x *. 40.) +. 80. *. sin (y /. 30.) /. (y /. 100.)
 
(* 3 parameter functions *)
let f7 () = 
  let x = float_of_int(!weights.(0)) in
  let y = float_of_int(!weights.(1)) in
  let z = float_of_int(!weights.(2)) in

  -500. *.  (sin x *. 30.) /. (x *. 40.) +. 80. *. sin (y /. 30.) /. (y /. 100.) +. z

(* duplicate list generator with size 10 and x = 5 *)
let duplicatelist () =
  let size = 10 in
  let x = 5 in
  Generators.Duplicatelist_freq.duplicate_list_gen size x

let evenlist () = 
  let size = 10 in
  Generators.Evenlist_freq.even_list_gen size

(* sizedlist generator with size 10 *)
let sizedlist () = 
  let size = 10 in
  Generators.Sizedlist_freq.sized_list_gen size

let sortedlist () =
  let size = 10 in
  let x = 5 in
  try Some (Generators.Sortedlist_freq.sorted_list_gen size x)
  with Combinators.BailOut -> None


(* rb tree generator
true = red 

inv - tree height is 4 or 5
color - red
h - black height is 2 (patrick uses max height 6) *)
let rbtree () = 
  let height = 2 in
  let color = true in
  let inv = if color then 2 * height else (2 * height) + 1 in
  Generators.Rbtree_freq.rbtree_gen inv color height


(* meta parameters *)
let iterations = 5000

let evaluate ?(print_stdout=true) (gen : unit -> 'a) (score_function : 'a list -> float -> float * float) (goal : float) (*(name : string)*) =

  (* run initial *)
  let test_oc = if (print_stdout) then
    stdout
  else
    let filename = "bin/results.csv" in
    open_out filename
  in

  let start_time = Unix.gettimeofday () in
  let results = collect 0 [] gen in
  let end_time : float = Unix.gettimeofday () in
  let init_dist, _ = score_function results 0. in


  (* print initial *)
  Printf.fprintf test_oc 
    "\nTest 1: Sizedlist_gen - dist of nil\nGoal: distr <= %.2f\nRan %d iterations\n\n" 
    goal iterations;
  Printf.fprintf test_oc "%8s %8s %8s\n" "" "dist" "time";
  Printf.fprintf test_oc "%s\n" (String.make 30 '-');
  (* Printf.fprintf test_oc "%8s" "Initial:"; *)
  Printf.fprintf test_oc "%8s %8.2f %8.4fs\n\n" "Initial:" init_dist (end_time -. start_time);


  (* run with adjustment *)
  (* weights := [|1000,1000,1000,1000|]; *)
  let oc = open_out "bin/iterations.csv" in 
  let (w, s, final_dist, time) = 
    random_restart
      oc
      rbtree
      score_rbtree_black
      goal
      iterations
      simulated_annealing
  in 
  close_out oc;

  (* Print final results *)
  Printf.fprintf test_oc "%8s %8.2f %8.4fs\n\n" "Final:" final_dist time

let () = 
  evaluate rbtree score_rbtree_black 0.4





