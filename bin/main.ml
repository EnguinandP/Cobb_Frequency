module Env = Zzenv
open Frequency_combinators
open Feature_vectors
open Stdlib

(* meta parameters *)
let iterations = 5000
let init_temp = 300.

(* 300 *)
(* scipy default temperature is 5230 *)

let sample_size = !sample (* Dragen sample size is 100000 *)
let step_size = 1
let step_range = (1, 20)
let n_reset = 10
let data_type = ref ""
let feature = ref ""
let usage_msg = "Usage: dune exec Cobb_Frequency <data_type> [-f] <program_file"
let set_data_type d = data_type := d

let speclist =
  [ ("-f", Arg.String (fun s -> data_type := s), "Set specific feature") ]

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

    let results = gen_value :: results in
    collect (n - 1) results gen

let rec collect_bailout n results gen =
  (* if !time_out_ref then raise Timed_out; *)
  if n = 0 then results
  else
    let gen_value =
      try gen ()
      with Combinators.BailOut ->
        print_endline "bail";
        []
    in

    let results = results + 1 in
    collect_bailout (n - 1) results gen

(** calculates the (dist, score, and time) given the goal dist, the generator,
    and the feature vector

    when target is only value *)

(** chi-square goodness of fit

    when multiple fv and goal pairs *)
let get_list_score fv goals gen results =
  (* let fv_ = [ (h_balanced_tree_fv, 1.5); (stick_tree_fv, 0.5) ] in *)
  let accumulate fv goal =
    let pass = List.fold_left fv 0. results in

    let dist = pass /. float_of_int (List.length results) in
    dist -. goal
  in

  let obs = List.map2 accumulate fv goals in

  (* [a, a, -1, a] *)
  let chi, n_none =
    List.fold_left2
      (fun (acc, n) o e ->
        if e = -1. then (acc, n)
        else
          ( (* Printf.printf "o %f e %f\n" o e; *)
            ((o -. e) *. (o -. e) /. e) +. acc,
            n + 1 ))
      (0., 0) obs goals
  in

  (* alpha = .05 and 4 degrees of freedom *)
  let crit = crit_vals.(n_none - 1) in
  ((chi, obs), chi -. crit)

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

let simulated_annealing (result_oc : out_channel) (gen : unit -> 'a) score_func
    goal (niter : int) print_all =
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

      let start_time = Unix.gettimeofday () in
      let results = collect sample_size [] gen in
      let end_time : float = Unix.gettimeofday () in

      (* calculates score *)
      let (dist, _), cand_score = score_func goal results in
      (* Printf.printf "s = %f b = %f\n" cand_score curr_score; *)

      print_iterations result_oc (string_of_int n) curr_weight cand_score dist
        (start_time -. end_time);

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
    loop 0 temp true !weights 10000000000. !weights 10000000000. 10000000000. 0
      None
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
let random_restart (result_oc : out_channel) (gen : unit -> 'b)
    (score_func : 'a -> 'b list -> (float * float list) * float) (goal : 'a)
    (niter : int) algor =
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
        algor result_oc gen score_func goal restart_interval false
      in

      (* Printf.printf "fin %d %f %f " n score dist;
      (* Array.iter (fun x -> Printf.printf "%d " x) weight; *)
      Printf.printf "\n"; *)

      (* returns top three *)
      let best_res = (weight, score, dist) :: best_res in
      let best_res =
        List.sort (fun (_, s1, _) (_, s2, _) -> compare s2 s1) best_res
      in
      let best_res =
        match best_res with
        | [ _; x1; x2; x3 ] -> [ x1; x2; x3 ]
        | _ -> best_res
      in

      restart (n + 1) best_res
  in

  let best_res = restart 0 [] in

  (* chi doesn't work here because the sample size increased *)
  (* recomputes best results with 100,000 *)
  let best_res =
    List.map
      (fun (weight, _, _) ->
        weights := weight;
        (* let old_sample = !sample in *)
        (* sample := 100000; *)
        let results = collect 1000 [] gen in
        let dist, score = score_func goal results in
        (* sample := old_sample; *)

        (* Printf.printf "\n %f  " score;
        Array.iter (fun x -> Printf.printf "%d " x) weight;
        Printf.printf "\n"; *)
        (weight, score, dist))
      best_res
  in

  (* sorts by top score *)
  let best_res =
    List.sort
      (fun (_, s1, _) (_, s2, _) ->
        let s11 = 0. -. s1 in
        let s22 = 0. -. s2 in
        compare s1 s2)
      best_res
  in

  (* List.iter (fun (_, s2, _) -> Printf.printf "%f " s2) best_res;
  print_newline (); *)
  let best_weight, best_score, best_dist = List.hd best_res in

  (* Printf.printf "%f\n" best_score; *)
  let best_dist, chi_pieces = best_dist in

  let end_time = Unix.gettimeofday () in
  let _ =
    print_iterations result_oc "solution" best_weight best_score best_dist
      (end_time -. start_time)
  in
  (* return lowest dist under goal or return closest to goal but under or returns under closest to goal  *)
  (best_weight, best_score, (best_dist, chi_pieces), end_time -. start_time)

let () = QCheck_runner.set_seed 42

(* maybe functorize evaluate? *)

(*let evaluate_list ?(test_oc = stdout) gen fv goals =
  let gen_name, g = gen in
  (* let fv_name, f = fv in *)
  (* run initial *)
  let (init_chi, init_dist), _, int_time =
    get_list_score fv sample_size goals g
  in

  (* print initial *)
  Printf.fprintf test_oc "\nTest: %s - " gen_name;
  List.iteri
    (fun i (fv_name, _) ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%s" fv_name)
    fv;
  Printf.fprintf test_oc "\nGoal: distr = ";

  List.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%.3f" x)
    goals;
  Printf.fprintf test_oc "\nRan %d iterations & %d restarts\n\n" iterations
    n_reset;
  Printf.fprintf test_oc "%16s %-35s %-10s %-10s %-10s\n" "" "dist" "chi" "time"
    "weights";
  Printf.fprintf test_oc "%s\n" (String.make 100 '-');
  Printf.fprintf test_oc "%-16s %s" "Initial:" "(";
  List.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%.3f" x)
    init_dist;
  Printf.fprintf test_oc ") %-5s %-10.3f %-10.4f %s" " " init_chi int_time "(";
  Array.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%d" x)
    !weights;
  Printf.fprintf test_oc ")\n";

  (* run with adjustment *)
  (* weights := [|1000,1000,1000,1000|]; *)
  let oc = open_out "bin/iterations.csv" in
  let w, s, (fin_chi, fin_dist), fin_time =
    random_restart oc g (get_list_score fv) goals iterations simulated_annealing
  in
  close_out oc;

  (* Print final results *)
  Printf.fprintf test_oc "%-16s %s" "Final:" "(";
  List.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%.3f" x)
    fin_dist;
  Printf.fprintf test_oc ") %-5s %-10.3f %-10.4f %s" " " fin_chi fin_time "(";
  Array.iteri
    (fun i x ->
      if i > 0 then Printf.fprintf test_oc ", ";
      Printf.fprintf test_oc "%d" x)
    !weights;
  Printf.fprintf test_oc ")\n"*)

let pp_res fmt
    ( gen_name,
      fv_name,
      goal,
      iterations,
      n_reset,
      init_dist,
      init_time,
      init_weights,
      fin_dist,
      fin_time,
      fin_weights ) =
  let open Format in
  let aux version dist time weights =
    fprintf fmt "%-16s" version;
    let _ =
      match dist with
      | dist', [] ->
          fprintf fmt "%-10.3f %27s %-7s %-10.3f %s" dist' "-" " " time "("
      | dist', chi_pieces ->
          fprintf fmt "(";
          List.iteri
            (fun i x ->
              if i > 0 then fprintf fmt ", ";
              fprintf fmt "%.3f" x)
            chi_pieces;
          fprintf fmt ") %-5s " " ";
          fprintf fmt "%-10.3f %-10.3f %s" dist' time "("
    in

    Array.iteri
      (fun i x ->
        if i > 0 then fprintf fmt ", ";
        fprintf fmt "%d" x)
      weights;
    fprintf fmt ")\n"
  in

  fprintf fmt "\nTest: %s - %s \n" gen_name fv_name;
  let _ =
    match goal with
    | g :: [] -> fprintf fmt "Goal: %.3f" g
    | [] -> failwith "printing error"
    | _ ->
        fprintf fmt "Goal: ";
        List.iteri
          (fun i x ->
            if i > 0 then fprintf fmt ", ";
            fprintf fmt "%.3f" x)
          goal
  in

  fprintf fmt "\nRan %d iterations & %d restarts\n\n" iterations n_reset;
  fprintf fmt "%16s %-35s %-10s %-10s %-10s\n" "" "dist" "chi" "time" "weights";
  fprintf fmt "%s\n" (String.make 100 '-');

  aux "Initial" init_dist init_time init_weights;
  aux "Final" fin_dist fin_time fin_weights

let print_csv oc
    ( gen_name,
      fv_name,
      goal,
      iterations,
      n_reset,
      init_dist,
      init_time,
      init_weights,
      fin_dist,
      fin_time,
      fin_weights ) =
  let aux version dist time weights =
    Printf.fprintf oc "%s,%s,%s," version gen_name fv_name;
    let _ =
      match goal with
      | g :: [] -> Printf.fprintf oc "%.3f," g
      | [] -> failwith "printing error"
      | _ ->
          Printf.fprintf oc "\"(";
          List.iteri
            (fun i x ->
              if i > 0 then Printf.fprintf oc ", ";
              Printf.fprintf oc "%.3f" x)
            goal;
          Printf.fprintf oc ")\","
    in

    let _ =
      match dist with
      | dist', [] -> Printf.fprintf oc "%.3f,\"(" dist'
      | dist', chi_pieces ->
          Printf.fprintf oc "\"(";
          List.iteri
            (fun i x ->
              if i > 0 then Printf.fprintf oc ", ";
              Printf.fprintf oc "%.3f" x)
            chi_pieces;
          Printf.fprintf oc ")\",\"("
    in

    Array.iteri
      (fun i x ->
        if i > 0 then Printf.fprintf oc ", ";
        Printf.fprintf oc "%d" x)
      weights;

    let _ =
      match dist with
      | dist', [] -> Printf.fprintf oc ")\",,"
      | dist', chi_pieces -> Printf.fprintf oc ")\", %.3f," dist'
    in
    Printf.fprintf oc "%.3f,%d,%d\n" time iterations n_reset
  in

  Printf.fprintf oc
    "version,generator,fv,goal,dist,weights,chi,time,iterations,restarts\n";
  aux "initial" init_dist init_time init_weights;
  aux "final" fin_dist fin_time fin_weights;
  ()

let print_table oc
    ( gen_name,
      n_weight,
      n_bool,
      n_nat,
      fv_name,
      goal,
      iterations,
      init_dist,
      fin_dist,
      fin_time ) =

  let dist_aux dist =
    match dist with
    | dist', [] -> Printf.fprintf oc ",%.3f" dist'
    | dist', chi_pieces ->
        Printf.fprintf oc "\"(";
        List.iteri
          (fun i x ->
            if i > 0 then Printf.fprintf oc ", ";
            Printf.fprintf oc "%.3f" x)
          chi_pieces;
        Printf.fprintf oc ")\""
  in

  Printf.fprintf oc "%s,%s,%d,%d,%d,%d," gen_name fv_name n_bool n_nat n_weight
    1;

  let _ =
    match goal with
    | g :: [] -> Printf.fprintf oc "%.3f," g
    | [] -> failwith "printing error"
    | _ ->
        Printf.fprintf oc "\"(";
        List.iteri
          (fun i x ->
            if i > 0 then Printf.fprintf oc ", ";
            Printf.fprintf oc "%.3f" x)
          goal;
        Printf.fprintf oc ")\","
  in

  dist_aux init_dist;
  dist_aux fin_dist;

  Printf.fprintf oc "%f,%d" fin_time iterations

let evaluate test_oc gen
    (fv : string * (float list -> 'a list -> (float * float list) * float))
    (goal_list : float list) =
  let gen_name, g, n_weights, n_bool, n_nat = gen in
  let fv_name, f = fv in

  let file_path =
    Printf.sprintf "bin/results/%s/%s_%s%d_%d.csv" gen_name fv_name
      (List.fold_left (fun acc x -> acc ^ string_of_float x ^ "_") "" goal_list)
      iterations n_reset
  in

  let results_oc = open_out file_path in

  weights := Array.init n_weights (fun _ -> 500);

  (* run initial *)
  let start_time = Unix.gettimeofday () in
  let results = collect sample_size [] g in
  let end_time = Unix.gettimeofday () in

  let init_dist, _ = f goal_list results in
  let init_weights = !weights in

  (* run with adjustment *)
  let oc = open_out "bin/iterations.csv" in
  let w, s, fin_dist, fin_time =
    random_restart oc g f goal_list iterations simulated_annealing
  in
  close_out oc;
  let fin_weights = !weights in

  pp_res Format.std_formatter
    ( gen_name,
      fv_name,
      goal_list,
      iterations,
      n_reset,
      init_dist,
      end_time -. start_time,
      init_weights,
      fin_dist,
      fin_time,
      fin_weights );

  print_csv results_oc
    ( gen_name,
      fv_name,
      goal_list,
      iterations,
      n_reset,
      init_dist,
      end_time -. start_time,
      init_weights,
      fin_dist,
      fin_time,
      fin_weights );

  (* print_table test_oc
    ( gen_name,
      n_weights,
      n_bool,
      n_nat,
      fv_name,
      goal_list,
      iterations,
      init_dist,
      fin_dist,
      fin_time ); *)
  ()

(* generators *)
let sortedlist_gen = ("sorted_list", sortedlist, 4, 0, 0)
let uniquelist_gen = ("unique_list", uniquelist, 0, 0, 0)
let sizedlist_gen = ("sized_list", sizedlist, 2, 1, 0)
let rbtree_gen = ("rb_tree", rbtree, 4, 2, 0)
let depthtree_gen = ("sized_tree", depthtree, 2, 1, 0)
let depthbst_gen = ("BST", depthbst, 0, 0, 0)
let dragen_gen = ("Dragen_tree", dragen_tree, 6, 2, 0)
let ld_rbtree_gen = ("Loaded_Dice_rbtree", ld_rbtree, 5 * 8, 0, 0)

(* feature vectors *)
let nil_list_fv = ("nil", get_exact_score nil_fv)
let min_nil_list_fv = ("nil_min", get_score nil_fv)
let len_list_fv = ("len", get_exact_score len_fv)
let min_len_list_fv = ("len_min", get_score len_fv)
let bail_list_fv = ("bailouts", get_score bailout_fv)
let b_rbtree_fv = ("black", get_score b_fv)
let height_tree_fv = ("height", get_score height_fv)
let stick_tree_fv = ("stick", get_score stick_fv)
let h_balanced_tree_fv = ("height_bal", get_score h_balanced_fv)
let count_cons = ("constructors", get_chi_score count_constr_list)
let uni_len_list_fv = ("len_uni", uniform_len_fv)
let uni_height_rbtree_fv = ("heigh_uni", uniform_height_fv)

(* (fv, goal) *)
let (sizedlist_tests :
      ((string * (float list -> 'a list list -> (float * float list) * float))
      * float list)
      list) =
  [ 
    (nil_list_fv, [ 0.1 ]);  
    (min_nil_list_fv, [ 0.1 ]);  
    (len_list_fv, [ 5. ]);
    (min_len_list_fv, [ 5. ]);
    (uni_len_list_fv, [10.]);
    ]

let sortedlist_tests = [ (bail_list_fv, [ 0.01 ]) ]
let evenlist_tests = []
let depthbst_tests = []
let rbtree_tests = [ (b_rbtree_fv, [ 0.2 ]); (b_rbtree_fv, [ 0.4 ]) ]

let depthtree_tests =
  [ (h_balanced_tree_fv, [ 1.5 ]); (stick_tree_fv, [ 0.5 ]) ]

let dragen_tests =
  [
    (count_cons, [ 10.; 10.; 10.; 10. ]);
    (* uniform *)
    (count_cons, [ 30.; 10.; 10.; -1. ]);
    (* weighted A *)
    (count_cons, [ 10.; -1.; -1.; 30. ]);
    (* weighted B *)
    (count_cons, [ 10.; 0.01; 0.01; 0.01 ]);
    (* only A *)
    (count_cons, [ -1.; -1.; 0.01; -1. ]) (* without A *);
  ]

type test =
  | List_type of
      ((string * (unit -> int list) * int * int * int)
      * ((string
         * (float list -> int list list -> (float * float list) * float))
        * float list)
        list)
  | List_opt_type of
      ((string * (unit -> int list option) * int * int * int)
      * ((string
         * (float list -> int list option list -> (float * float list) * float))
        * float list)
        list)
  | Rb_type of
      ((string * (unit -> int Combinators.rbtree) * int * int * int)
      * ((string
         * (float list ->
           int Combinators.rbtree list ->
           (float * float list) * float))
        * float list)
        list)
  | Depthtree_type of
      ((string * (unit -> int Combinators.tree) * int * int * int)
      * ((string
         * (float list ->
           int Combinators.tree list ->
           (float * float list) * float))
        * float list)
        list)
  | Dragen_type of
      ((string * (unit -> Frequency_combinators.dragen_tree) * int * int * int)
      * ((string
         * (float list ->
           Frequency_combinators.dragen_tree list ->
           (float * float list) * float))
        * float list)
        list)

let tests =
  [
    ("sized_list", List_type (sizedlist_gen, sizedlist_tests));
    ("rb_tree", Rb_type (rbtree_gen, rbtree_tests));
    ("depth_tree", Depthtree_type (depthbst_gen, depthtree_tests));
    ("dragen_tree", Dragen_type (dragen_gen, dragen_tests));
    ("sorted_list", List_opt_type (sortedlist_gen, sortedlist_tests));
  ]

let evaluate_test test_list oc =
  match test_list with
  | List_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate oc g fv goal)
        fvl
  | List_opt_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate oc g fv goal)
        fvl
  | Rb_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate oc g fv goal)
        fvl
  | Depthtree_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate oc g fv goal)
        fvl
  | Dragen_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate oc g fv goal)
        fvl

let () =
  Arg.parse speclist set_data_type usage_msg;

  let table_oc = open_out "bin/tables/table1.csv" in
  Printf.fprintf table_oc
    "data type,feature \
     vector,#bool_gen,#nat_gen,#weights,#features,target,start dist,end \
     dist,time,iterations\n";

  let test =
    match List.assoc_opt !data_type tests with
    | Some s -> s
    | None -> failwith "unknown test"
  in

  let res = evaluate_test test table_oc in

  let fv_ = [ (h_balanced_tree_fv, 1.5); (stick_tree_fv, 0.5) ] in

  (* Kolmogorov–Smirnov test / make buckets *)

  (* in dragen, weights were distr * size *)
  let uniform1 = [ 10.; 10.; 10.; 10. ] in
  let weighted_A = [ 30.; 10.; 10.; -1. ] in
  let weighted_B = [ 10.; -1.; -1.; 30. ] in
  let only_leafA = [ 10.; 0.01; 0.01; 0.01 ] in
  let without_leafC = [ -1.; -1.; 0.01; -1. ] in

  let uniform_exp = [ 5.26; 5.26; 5.21; 14.73 ] in
  let weight_A_exp = [ 30.07; 9.76; 10.15; 48.96 ] in
  let weight_B_exp = [ 10.07; 3.15; 17.57; 29.80 ] in
  let only_leafA_exp = [ 10.41; 0.01; 0.01; 9.41 ] in
  let without_leafC_exp = [ 6.95; 6.95; 0.01; 12.91 ] in

  (* weights := [| 500; 500; 500; 500; 500; 500 |];
  evaluate dragen_gen count_cons weight_A_exp ~test_oc:result_oc; *)

  (* weights := Array.init (5 * 8) (fun _ -> 500);
  evaluate ld_rbtree_gen uniform_height_rbtree [ 5.0 ] ~test_oc:result_oc; *)

  (* weights := [| 500; 500 |];
  evaluate sizedlist_gen uniform 10. ~test_oc:result_oc; *)
  (* weights := [| 500; 500 |];
  evaluate uniquelist_gen len_list_fv 5. ~test_oc:result_oc; *)
  (* weights := [| 500; 500 |];
  evaluate uniquelist_gen bail_list_fv 5. ~test_oc:result_oc; *)
  close_out table_oc
