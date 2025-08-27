module Env = Zzenv
open Frequency_combinators
open Feature_vectors
open Stdlib

(* meta parameters *)
let iterations = 1000
let init_temp = 300. (* 300 *)

(* scipy default temperature is 5230 *)
let sample_size = 1000
let step_size = 1
let step_range = (1, 20)
let n_reset = 5

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

(* type brPrTree = LeafA | LeafB | LeafC | Node of brPrTree * brPrTree

let rec count_constr_list tree =
  match tree with
  | LeafA -> [ 1.; 0.; 0.; 0. ]
  | LeafB -> [ 0.; 1.; 0.; 0. ]
  | LeafC -> [ 0.; 0.; 1.; 0. ]
  | Node (lt, rt) ->
      let ltc = count_constr_list lt in
      let rtc = count_constr_list rt in
      List.map2 ( +. ) ltc
        (List.mapi (fun i x -> if i = 3 then x +. 1. else x) rtc) *)

(** collects n values with gen *)
let rec collect n results gen =
  (* if !time_out_ref then raise Timed_out; *)
  if n = 0 then results
  else
    let gen_value = gen () in

    (* collects generated values in results *)
    let results = gen_value :: results in
    collect (n - 1) results gen

(** calculates the (dist, score, and time) given the goal dist, the generator,
    and the feature vector *)
let get_score size goal gen fv =
  let start_time = Unix.gettimeofday () in
  let results = collect size [] gen in
  let end_time : float = Unix.gettimeofday () in

  let pass = List.fold_left fv 0. results in

  let dist = pass /. float_of_int (List.length results) in
  (dist, dist -. goal, end_time -. start_time)

(** chi *)
let get_chi_score size exp gen count =
  let start_time = Unix.gettimeofday () in
  let results = collect size [] gen in
  let end_time : float = Unix.gettimeofday () in

  (* avg *)
  let obs = List.map count results in
  let obs =
    List.fold_left
      (fun acc x -> List.map2 ( +. ) acc x)
      (List.init 4 (fun _ -> 0.))
      obs
  in
  let obs = List.map (fun x -> x /. float_of_int (List.length results)) obs in

  let chi =
    List.fold_left2
      (fun acc o e -> ((o -. e) *. (o -. e) /. e) +. acc)
      0. obs exp
  in

  let crit = 9.488 in
  (chi, chi -. crit, end_time -. start_time)

let exit_cond dist goal = dist <= goal

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

  if Random.bool () then !weights.(direction) <- cand_weight.(direction) + step
  else if step >= cand_weight.(0) then ()
  else !weights.(direction) <- cand_weight.(direction) - step;
  !weights

let step_n_param (cand_weight : int array) =
  let step = Random.int (snd step_range) + fst step_range in

  let n = Array.length cand_weight in
  let direction = Random.int n in

  (* this is much harder to read *)
  let s =
    if Random.bool () then step
    else if
      (* weight can't be negative *)
      step >= cand_weight.(direction)
    then 0
    else step * -1
  in
  !weights.(direction) <- cand_weight.(direction) + s;
  weights

(* updates the temperature *)
let update_temp n = init_temp /. (float_of_int n +. 1.)

(** prints one iteration of results as "n, curr_weights, cand weight, cand
    weights, score, dist, time" *)
let print_iterations oc n curr_weight cand_score dist time =
  Printf.fprintf oc "%s," n;
  Array.iter (fun x -> Printf.fprintf oc "%d," x) curr_weight;
  Array.iter (fun x -> Printf.fprintf oc "%d," x) !weights;
  let _ = Printf.fprintf oc "%f,%f,%f\n" cand_score dist time in
  ()

(** prints final solution *)
let print_solutions oc best_weights best_score =
  match oc with
  | None -> ()
  | Some c -> print_iterations c "solution" best_weights best_score 0. 0.

(** prints header of csv file *)
let print_header oc =
  match oc with
  | None -> ()
  | Some c ->
      Printf.fprintf c "iteration,";
      Array.iteri
        (fun i x -> Printf.fprintf c "curr weight %d," (i + 1))
        !weights;
      Array.iteri
        (fun i x -> Printf.fprintf c "cand weight %d," (i + 1))
        !weights;
      let _ = Printf.fprintf c "score,distribution,time\n" in
      ()

let time_out_ref = ref false

(* exception Timed_out *)

let () =
  Core.Signal.Expert.handle Core.Signal.alrm (fun (_ : Core.Signal.t) ->
      Printf.printf "Timed out";
      time_out_ref := true)

let simulated_annealing (result_oc : out_channel) (gen : unit -> 'a) fv
    score_func goal (niter : int) print_all =
  let temp = init_temp in

  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  let rec loop n temp direction curr_weight curr_score best_weight best_score
      (best_dist : float) count spare =
    if n = niter (*|| exit_cond best_dist goal*) then
      (best_weight, best_score, best_dist)
    else
      let _ = step_n_param curr_weight in

      let temp = update_temp n in

      (* calculates score *)
      let dist, cand_score, time = score_func sample_size goal gen fv in

      print_iterations result_oc (string_of_int n) curr_weight cand_score dist
        time;

      (* when score is worse, e^+ -> true *)
      if Random.float 1.0 < Float.exp (-.(cand_score -. curr_score) /. temp)
      then
        (* keep weight just tested *)
        (* let _ = Printf.fprintf result_oc "ACCEPT " in *)
        let w = Array.map (fun r -> r) !weights in
        if cand_score < best_score then
          loop (n + 1) temp direction w cand_score w cand_score dist count spare
        else
          loop (n + 1) temp direction w cand_score best_weight best_score
            best_dist count spare
      else
        (* let _ = Printf.fprintf result_oc "REJECT " in *)
        loop (n + 1) temp (not direction) curr_weight curr_score best_weight
          best_score best_dist (count + 1) spare
  in

  let best_weight, best_score, best_dist =
    loop 0 temp true !weights 1. !weights 1000. 1000. 0 None
  in
  let _ = print_solutions extra_oc best_weight best_score in

  (best_weight, best_score, best_dist)

let basin_hoppping (result_oc : out_channel) gen calc_score (niter : int) goal
    print_all =
  (* initial solution is stored in the ref *)
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  (* minimize is local min search *)
  let rec minimize n best_weight best_score best_dist spare =
    (* next step *)
    let _ = step_n_param best_weight in
    let results = collect sample_size [] gen in
    let dist, next_score = calc_score results goal in
    (* let results = gen () in
    let next_score, spare, dist = calc_noisy_score spare results feature_vector in *)

    if n > 100 then (best_weight, best_score, best_dist)
    else
      (* if score is closer to min, then take step *)
      let w = Array.map (fun r -> r) !weights in
      if next_score < best_score then minimize (n + 1) w next_score dist spare
      else minimize (n + 1) best_weight best_score best_dist spare
  in

  let rec loop curr_weight curr_min best_weight best_score best_dist n spare =
    if n > niter || exit_cond best_dist goal then
      (best_weight, best_score, best_dist)
    else
      let temp = update_temp n in

      (* perturbation (step) *)
      let _ = step_in_range curr_weight (30, 50) in
      (* try adaptive stepwise *)

      (* let start_time = Unix.gettimeofday () in
      (* let results = gen () in *)
      let results = collect 0 [] gen in
      let end_time : float = Unix.gettimeofday () in
      let _, _ = calc_score results feature_vector in *)
      (* let _, spare, dist = calc_noisy_score spare results goal feature_vector in *)

      (* minimize *)
      let next_min_weight, next_min, next_dist =
        minimize 0 (Array.map (fun r -> r) !weights) 100000. 1. spare
      in
      (* Printf.printf "%d %f %f\n" !weights1.(0) next_score next_min; *)

      let _ =
        print_iterations result_oc (string_of_int n) next_min_weight next_min
          next_dist 0.
      in
      (* Printf.printf "%d,%d,%d,%f,%f,0,0\n" n curr_weight.(0) !weights.(0) next_min next_dist; *)

      (* acceptance test *)
      if Random.float 1.0 < Float.exp (-.(next_min -. curr_min) /. temp) then
        (* let w = Array.map (fun r -> r) !weights in *)
        if next_min < best_score then
          loop next_min_weight next_min next_min_weight next_min next_dist
            (n + 1) spare
        else
          loop next_min_weight next_min best_weight best_score best_dist (n + 1)
            spare
      else
        loop curr_weight curr_min best_weight best_score best_dist (n + 1) spare
  in

  (* min is intial solution *)
  let init_weight, init_min, init_dist =
    minimize 0 (Array.map (fun r -> r) !weights) 100000. 1. None
  in
  let best_weights, best_score, best_dist =
    loop init_weight init_min init_weight init_min 1. 0 None
  in
  let _ = print_solutions extra_oc best_weights best_score in

  (best_weights, best_score, best_dist)

(** uses random restart with specified optimization algorithm *)
let random_restart (result_oc : out_channel) gen fv score_func
    (goal : float list) (niter : int) algor =
  let start_time = Unix.gettimeofday () in

  let restart_interval = niter / n_reset in

  (* let result_oc = open_out output in *)
  print_header (Some result_oc);

  let rec restart n (best_res : (int array * float * float) list) :
      (int array * float * float) list =
    if n >= n_reset then best_res
    else
      (* new location between 0 and 1000 *)
      let new_start = Array.map (fun _ -> Random.int 1000) !weights in
      weights := new_start;
      (* Printf.printf "\n%d\n" !weights.(0); *)
      let weight, score, dist =
        algor result_oc gen fv score_func goal restart_interval false
      in

      print_int n;
      Printf.printf " %d %f %f " n score dist;
      Array.iter (fun x -> Printf.printf "%d " x) weight;
      Printf.printf "\n";

      (* returns top three *)
      let best_res = (weight, score, dist) :: best_res in
      let best_res =
        List.sort
          (* (fun (_, s1, _) (_, s2, _) ->
            if s1 > 0. && s2 > 0. then compare s2 s1
            else if s1 > 0. then -1
            else if s2 > 0. then 1
            else compare s1 s2) *)
          (fun (_, s1, _) (_, s2, _) -> compare s2 s1)
          best_res
      in
      let best_res =
        match best_res with
        | [ _; x1; x2; x3 ] -> [ x1; x2; x3 ]
        | _ -> best_res
      in

      restart (n + 1) best_res
  in

  let l = restart 0 [] in
  let l =
    List.map
      (fun (weight, _, _) ->
        (* sample size used in Branching Processes paper *)
        weights := weight;
        let d, s, t = get_chi_score 100000 goal gen fv in
        (* Array.iter (fun x -> Printf.printf "%d " x) weight;
        Printf.printf "%f %f\n" s d; *)
        (weight, s, d))
      l
  in

  let l =
    List.sort
      (fun (_, s1, _) (_, s2, _) ->
        if s1 > 0. && s2 > 0. then compare s1 s2
        else if s1 > 0. then 1
        else if s2 > 0. then -1
        else compare s2 s1)
      (* (fun (_, s1, _) (_, s2, _) -> compare s1 s2) *)
      l
  in
  let best_weight, best_score, best_dist = List.hd l in

  List.iter (fun (_, _, d) -> Printf.printf "%f " d) l;
  print_newline ();

  let end_time = Unix.gettimeofday () in
  let _ =
    print_iterations result_oc "solution" best_weight best_score best_dist
      (end_time -. start_time)
  in
  (* return lowest dist under goal or return closest to goal but under or returns under closest to goal  *)
  (best_weight, best_score, best_dist, end_time -. start_time)

let () = QCheck_runner.set_seed 42

(* maybe functorize evaluate? *)
let evaluate_chi ?(test_oc = stdout) gen fv (goal : float list) =
  let name1, g = gen in
  let name2, f = fv in
  (* run initial *)
  let init_dist, _, int_time = get_chi_score sample_size goal g f in

  (* print initial *)
  (* Printf.fprintf test_oc
    "\nTest: %s - %s \nGoal: distr <= %.3f\nRan %d iterations %d restarts\n\n"
    name1 name2 goal iterations n_reset; *)
  Printf.fprintf test_oc "%16s %-10s %-10s %-10s\n" "" "dist" "time" "weights";
  Printf.fprintf test_oc "%s\n" (String.make 50 '-');
  Printf.fprintf test_oc "%-16s %-10.3f %-10.4f %s" "Initial:" init_dist
    int_time "(";
  Array.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%d" x)
    !weights;
  Printf.fprintf test_oc ")\n";

  (* run with adjustment *)
  (* weights := [|1000,1000,1000,1000|]; *)
  let oc = open_out "bin/iterations.csv" in
  let w, s, fin_dist, fin_time =
    random_restart oc g f get_chi_score goal iterations simulated_annealing
  in
  close_out oc;

  (* Print final results *)
  Printf.fprintf test_oc "%-16s %-10.3f %-10.4f %s" "Final:" fin_dist fin_time
    "(";
  Array.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%d" x)
    !weights;
  Printf.fprintf test_oc ")\n"

(*let evaluate ?(test_oc = stdout) gen (fv : string * (float -> 'a -> float))
    (goal : float) =
  let name1, g_ = gen in
  let name2, f_ = fv in
  (* run initial *)
  let init_dist, _, int_time = get_score sample_size 0. g_ f_ in

  (* print initial *)
  Printf.fprintf test_oc
    "\nTest: %s - %s \nGoal: distr <= %.3f\nRan %d iterations %d restarts\n\n"
    name1 name2 goal iterations n_reset;
  Printf.fprintf test_oc "%16s %-10s %-10s %-10s\n" "" "dist" "time" "weights";
  Printf.fprintf test_oc "%s\n" (String.make 50 '-');
  Printf.fprintf test_oc "%-16s %-10.3f %-10.4f %s" "Initial:" init_dist
    int_time "(";
  Array.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%d" x)
    !weights;
  Printf.fprintf test_oc ")\n";

  (* run with adjustment *)
  (* weights := [|1000,1000,1000,1000|]; *)
  let oc = open_out "bin/iterations.csv" in
  let w, s, fin_dist, fin_time =
    random_restart oc g_ f_ get_score goal iterations simulated_annealing
  in
  close_out oc;

  (* Print final results *)
  Printf.fprintf test_oc "%-16s %-10.3f %-10.4f %s" "Final:" fin_dist fin_time
    "(";
  Array.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%d" x)
    !weights;
  Printf.fprintf test_oc ")\n"*)

let sizedlist_gen = ("sizedlist", sizedlist)
let rbtree_gen = ("rbtree", rbtree)
let brPrTree_gen = ("brPrTree", brPrTree)
let nil_list_fv = ("percent of lists that are nil", nil_fv)
let len_list_fv = ("avg len of list", len_fv)
let b_rbtree_fv = ("percent of black nodes in a tree", b_fv)
let leafa_fv = ("percent of leafa", leafa_fv)
let count_cons = ("constructors", count_constr_list)

let () =
  let result_oc = open_out "bin/results/result" in
  (* let result_oc = stdout in *)
  let init_weight = [| 500; 500 |] in

  (* weights := init_weight;
  evaluate sizedlist_gen nil_list_fv 0.1 ~test_oc:result_oc; *)
  (* weights := [| 100; 800 |];
  evaluate sizedlist_gen len_list_fv 2. ~test_oc:result_oc; *)
  (* weights := [| 500; 500; 500; 500 |];
  evaluate rbtree_gen b_rbtree_fv 0.2 ~test_oc:result_oc; *)
  (* weights := [| 500; 500; 500; 500 |];
  evaluate rbtree_gen b_rbtree_fv 0.4 ~test_oc:result_oc; *)
  (* weights := [| 500; 500; 500; 500; 500; 500 |];
  evaluate brPrTree_gen leafa_fv 0.0 ~test_oc:result_oc; *)
  (* Kolmogorov–Smirnov test / make buckets*)
  let uniform = [ 5.26; 5.26; 5.21; 14.73 ] in
  (* how to create dist? *)
  let weight_A = [ 30.07; 9.76; 10.15; 48.96 ] in
  let weight_B = [ 10.07; 3.15; 17.57; 29.80 ] in
  let only_leafA = [ 10.41; 0.; 0.; 9.41 ] in
  let without_leafC = [ 6.95; 6.95; 0.; 12.91 ] in

  weights := [| 500; 500; 500; 500; 500; 500 |];
  evaluate_chi brPrTree_gen count_cons without_leafC ~test_oc:result_oc;
  close_out result_oc
