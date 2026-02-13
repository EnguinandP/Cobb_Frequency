module Env = Zzenv
open Frequency_combinators
open Feature_vectors
open Stdlib

(* meta parameters *)
let iterations = ref 2000
let total_iterations = ref 0
let total_restarts = ref 0
let init_temp = 1000.
let temp = ref init_temp

(* 300 *)
(* scipy default temperature is 5230 *)

let sample_size = !sample (* Dragen sample size is 100000 *)
let step_size = 1
let step_range = ref (1, 40) (* step max starts at 40 and decreases to 20 *)
let n_reset = ref 20
let data_type = ref ""
let feature_vector = ref ""
let path = ref "./bin/results/parametrized_enumeration/sized_list_10/output.csv"
let top_oc = open_out !path
let search_strat_str = ref "sa"
let print_one = ref false

let usage_msg =
  "Usage: dune exec Cobb_Frequency <data_type> [-i] [-r] [-one] [-s]"

let set_data_type d = data_type := d

let speclist =
  [
    ("-i", Arg.Set_int iterations, "Set iterations");
    ("-r", Arg.Set_int n_reset, "Set random restarts");
    ("-f", Arg.Set_string feature_vector, "run with specified feature vector");
    ("-one", Arg.Set print_one, "print one in one file");
    ("-s", Arg.Set_string search_strat_str, "Set search strategy: di, dir, sa");
  ]

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
let collect n gen = List.init n (fun _ -> gen ())

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

let step (cand_weight : int array) =
  let step = Random.int (snd !step_range) + fst !step_range in

  let n = Array.length cand_weight in
  let direction = Random.int n in

  (* this is much harder to read *)
  let s =
    if Random.bool () then step
    else if (* weight can't be negative *)
            step >= cand_weight.(direction) then 0
    else step * -1
  in
  !weights.(direction) <- cand_weight.(direction) + s;
  weights

let step_neg (cand_weight : int array) =
  let step = Random.int (snd !step_range) + fst !step_range in

  let n = Array.length cand_weight in
  let direction = Random.int n in

  let s = if Random.bool () then step else step * -1 in
  !weights.(direction) <- cand_weight.(direction) + s;
  weights

(* updates the temperature *)
let update_temp n = temp := init_temp /. (float_of_int n +. 1.)

let update_step_range () =
  let step_max = 20. *. (2. ** ((!temp -. 1.) /. init_temp)) in
  (* Printf.printf " %f " (Float.log 2.); *)
  step_range := (fst !step_range, int_of_float step_max)

(** prints one iteration of results as "n, curr_weights, cand weight, cand weights, score, dist, time" *)
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

let dumb_iterate (result_oc : out_channel) (gen : unit -> 'a) score_func goal
    print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  let weights_list = [ 1; 100; 200; 300; 400; 500; 600; 700; 800; 900; 1000 ] in

  let n_weights = Array.length !weights in

  let sample_weights (w : int array) =
    weights := w;

    let collect_start_time = Unix.gettimeofday () in
    let results = collect sample_size gen in
    let collect_end_time = Unix.gettimeofday () in

    let (dist, chi_buckets), cand_score = score_func goal results in

    (* print_iterations result_oc "0" [| 0; 0 |] cand_score dist
       (collect_end_time -. collect_start_time); *)
    (cand_score, dist, chi_buckets)
  in

  let min_score (score_a, dist_a, chi_buckets_a, weights_a)
      (score_b, dist_b, chi_buckets_b, weights_b) =
    if score_a < score_b then (score_a, dist_a, chi_buckets_a, weights_a)
    else (score_b, dist_b, chi_buckets_b, weights_b)
  in

  let buffer = Array.make n_weights 0 in

  let rec iterate_list depth best =
    if depth = n_weights then
      let score_b, dist_b, chi_buckets_b = sample_weights buffer in
      let w = Array.map (fun r -> r) buffer in
      min_score best (score_b, dist_b, chi_buckets_b, w)
    else
      List.fold_left
        (fun acc elem ->
          buffer.(depth) <- elem;
          let result = iterate_list (depth + 1) acc in
          result)
        best weights_list
  in

  let start_time = Unix.gettimeofday () in
  let best_score, best_dist, best_chi_buckets, best_weights =
    iterate_list 0 (max_float, 0., [], [| 0 |])
  in
  let end_time = Unix.gettimeofday () in

  let _ = print_solutions extra_oc best_weights best_score in

  ( best_weights,
    best_score,
    (best_dist, best_chi_buckets),
    end_time -. start_time )

let dumb_iterate_ratios (result_oc : out_channel) (gen : unit -> 'a) score_func
    goal print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  (* let ratios_list = [ 1.; 0.9; 0.8; 0.7; 0.6; 0.5; 0.4; 0.2; 0.1; 0. ] in *)
  (* let ratios_list = [ 1.; 0.75; 0.5; 0.25; 0. ] in *)
  (* these are strictly the ratios that appear 1 or more times from weights *)
  (* let ratios_list = [ 0.; 0.12; 0.14; 0.17; 0.29; 0.3; 0.5; 0.53; 1.0; ] in  *)
  (* above plus a bonus *)
  let ratios_list = [ 0.; 0.15; 0.3; 0.5; 0.9; 1.0 ] in

  let weights_list =
    [| 1; 100; 200; 300; 400; 500; 600; 700; 800; 900; 1000 |]
  in

  let n_bool = Array.length !weights in

  let sample_weights (w : int array) =
    weights := w;

    let collect_start_time = Unix.gettimeofday () in
    let results = collect sample_size gen in
    let collect_end_time = Unix.gettimeofday () in

    let (dist, chi_buckets), cand_score = score_func goal results in

    print_iterations result_oc "0" [| 0; 0 |] cand_score dist
      (collect_end_time -. collect_start_time);

    (cand_score, dist, chi_buckets)
  in

  let min_score (score_a, dist_a, chi_buckets_a, weights_a)
      (score_b, dist_b, chi_buckets_b, weights_b) =
    if score_a < score_b then (score_a, dist_a, chi_buckets_a, weights_a)
    else (score_b, dist_b, chi_buckets_b, weights_b)
  in

  let buffer = Array.make n_bool 0 in

  let rec iterate_list depth best =
    if depth >= n_bool then
      let score_b, dist_b, chi_buckets_b = sample_weights buffer in
      let w = Array.map (fun r -> r) buffer in
      min_score best (score_b, dist_b, chi_buckets_b, w)
    else
      List.fold_left
        (fun acc r ->
          if not (depth + 1 = n_bool) then (
            let w1 = Int.of_float (r *. 100.) in
            let w2 = Int.of_float ((1. -. r) *. 100.) in

            buffer.(depth) <- w1;
            buffer.(depth + 1) <- w2;
            let result = iterate_list (depth + 2) acc in
            result)
          else
            let w1 = Int.of_float (r *. 100.) in

            let i = List.find_index (fun x -> x = r) ratios_list in
            let i =
              match i with Some i' -> i' | None -> failwith "index error"
            in
            buffer.(depth) <- weights_list.(i);
            let result = iterate_list (depth + 1) acc in
            result)
        best ratios_list
  in

  let start_time = Unix.gettimeofday () in
  let best_score, best_dist, best_chi_buckets, best_weights =
    iterate_list 0 (max_float, 0., [], [| 0 |])
  in
  let end_time = Unix.gettimeofday () in

  let _ = print_solutions extra_oc best_weights best_score in

  ( best_weights,
    best_score,
    (best_dist, best_chi_buckets),
    end_time -. start_time )

let simulated_annealing (result_oc : out_channel) (gen : unit -> 'a) score_func
    goal (niter : int) neg_w print_all =
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  let rec loop n curr_weight curr_score best_weight best_score best_dist
      best_chi_buckets =
    if n = !iterations || best_score <= 0. then
      let _ = Printf.printf "iter = %d\n" n in
      (best_weight, best_score, best_dist, best_chi_buckets, n)
    else
      let _ = if neg_w then step_neg curr_weight else step curr_weight in

      total_iterations := !total_iterations + 1;

      update_temp n;
      update_step_range ();

      try
        let start_time = Unix.gettimeofday () in
        let results = collect sample_size gen in
        let end_time : float = Unix.gettimeofday () in

        (* calculates score *)
        let (dist, chi_buckets), cand_score = score_func goal results in

        (* Printf.printf "s = %f b = %f\n" cand_score curr_score; *)
        print_iterations result_oc (string_of_int n) curr_weight cand_score dist
          (end_time -. start_time);

        (* when score is worse, e^+ -> true *)
        if Random.float 1.0 < Float.exp (-.(cand_score -. curr_score) /. !temp)
        then
          (* keep weight just tested *)
          (* let _ = Printf.fprintf result_oc "ACCEPT " in *)
          let w = Array.map (fun r -> r) !weights in
          if cand_score < best_score then
            loop (n + 1) w cand_score w cand_score dist chi_buckets
          else
            loop (n + 1) w cand_score best_weight best_score best_dist
              best_chi_buckets
        else
          (* let _ = Printf.fprintf result_oc "REJECT " in *)
          (* let _ = Printf.printf "reject %f %f\n" cand_score curr_score in *)
          loop (n + 1) curr_weight curr_score best_weight best_score best_dist
            best_chi_buckets
        (* if neg wait, reject *)
      with Neg_Weight s ->
        (* Printf.printf "\nskip %s\n" s; *)
        loop (n + 1) curr_weight curr_score best_weight best_score best_dist
          best_chi_buckets
  in

  let best_weight, best_score, best_dist, best_chi_buckets, n =
    loop 0 !weights 10000000000. !weights 10000000000. 10000000000. []
  in
  let _ = print_solutions extra_oc best_weight best_score in

  (best_weight, best_score, best_dist, best_chi_buckets, n)

let basin_hoppping (result_oc : out_channel) gen calc_score (niter : int) goal
    print_all =
  (* initial solution is stored in the ref *)
  let extra_oc = if print_all then Some result_oc else None in
  print_header extra_oc;

  (* minimize is local min search *)
  let rec minimize n best_weight best_score best_dist spare =
    (* next step *)
    let _ = step best_weight in
    let results = collect sample_size gen in
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
      let () = update_temp n in

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
      if Random.float 1.0 < Float.exp (-.(next_min -. curr_min) /. !temp) then
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
    (niter : int) neg_w
    algor
      (* : out_channel ->
         (unit -> 'a) ->
         (float list -> 'a list -> (float * float list) * float) ->
         float list -> int -> bool -> int array * float * float * float list *)
    =
  let start_time = Unix.gettimeofday () in

  let restart_interval = niter / !n_reset in

  (* let result_oc = open_out output in *)
  print_header (Some result_oc);

  let rec restart n niter
      best_res (* : (int array * float * float * 'a list) list *) =
    let length = List.length best_res in
    let score =
      match best_res with
      | [ (_, s, _, _, _); _; _; _; _ ] when s <= 0. -> s
      | _ -> Float.max_float
    in

    (* Printf.printf "s = %f\n" score; *)
    if n >= !n_reset || score <= 0. then
      (* let _ = Printf.printf "actual restarts = %d\n" n in *)
      best_res
    else
      (* new location between 0 and 1000 *)
      let new_start = Array.map (fun _ -> Random.int 1000) !weights in
      weights := new_start;
      temp := init_temp;

      total_restarts := !total_restarts + 1;

      let a =
        algor result_oc gen score_func goal restart_interval neg_w false
      in

      let _, _, _, _, iterations = a in
      (* returns top 5 *)
      let best_res = a :: best_res in
      let best_res =
        List.sort
          (fun (_, s1, _, _, _) (_, s2, _, _, _) -> compare s2 s1)
          best_res
      in

      let best_res =
        match best_res with
        | [ _; x1; x2; x3; x4; x5 ] -> [ x1; x2; x3; x4; x5 ]
        | _ -> best_res
      in

      restart (n + 1) (niter + iterations) best_res
  in

  let best_res = restart 0 0 [] in

  (* chi doesn't work here because the sample size increased *)
  (* recomputes best results with 100,000 *)
  let best_res =
    List.map
      (fun (weight, score, dist, chi_buckets, _) ->
        weights := weight;
        (* let old_sample = !sample in *)
        (* sample := 100000; *)
        try
          let results = collect 100000 gen in
          let precise_dist, precise_score = score_func goal results in

          (* sample := old_sample; *)
          let c, pieces = precise_dist in

          (* Printf.printf "\n %f vs %f d= %f " precise_score score dist; *)
          (* Printf.printf "\n d = %f " dist; *)
          (* Printf.printf "\n chi = %f  " c; *)
          (* Array.iter (fun x -> Printf.printf "%d " x) weight;
             Array.iter (fun (x, _) -> Printf.printf "%f " x) precise_dist;
             Printf.printf "\n"; *)
          (weight, precise_score, precise_dist, score, dist, chi_buckets)
        with Neg_Weight _ ->
          print_endline "neg weight\n";
          (weight, Float.max_float, (dist, [ 0. ]), score, dist, chi_buckets))
      best_res
  in

  (* sorts by lowest score *)
  let best_res =
    List.sort
      (fun (_, s1, _, _, _, _) (_, s2, _, _, _, _) ->
        let s11 = 0. -. s1 in
        let s22 = 0. -. s2 in
        compare s1 s2)
      best_res
  in

  let best_weight, _, _, best_score, best_dist, chi_buckets =
    List.hd best_res
  in

  let end_time = Unix.gettimeofday () in
  let _ =
    print_iterations result_oc "solution" best_weight best_score best_dist
      (end_time -. start_time)
  in

  (* return lowest dist under goal or return closest to goal but under or returns under closest to goal  *)
  (best_weight, best_score, (best_dist, chi_buckets), end_time -. start_time)

let () = QCheck_runner.set_seed 42

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

  fprintf fmt "\n\nRan %d iterations & %d restarts\n\n" iterations n_reset;
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
      init_score,
      fin_dist,
      fin_time,
      fin_weights,
      fin_score ) =
  let aux version dist time weights score =
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
    Printf.fprintf oc "%.3f," score;
    Printf.fprintf oc "%.3f,%d,%d\n" time iterations n_reset
  in

  Printf.fprintf oc
    "version,generator,fv,goal,dist,weights,chi,score,time,iterations,restarts\n";
  aux "initial" init_dist init_time init_weights init_score;
  aux "final" fin_dist fin_time fin_weights fin_score;
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

let evaluate gen
    (fv : string * (float list -> 'a list -> (float * float list) * float))
    (goal_list : float list) =
  let gen_name, g, n_weights, n_bool, n_nat = gen in
  let fv_name, f = fv in

  (* let gen_name = "dumb_iterate_ratios/" ^ gen_name in *)
  total_iterations := 0;
  total_restarts := 0;

  let gen_name =
    (match !search_strat_str with
    | "sa" -> ""
    | "di" -> "dumb_iterate_weights/"
    | "dir" -> "dumb_iterate_ratios/"
    | "dirs" -> "dumb_iterate_ratios_smaller/"
    | "dirw" -> "dumb_iterate_ratios_w2/"
    | _ -> failwith "invalid search stragtegy/")
    ^ gen_name
  in

  let file_path =
    Printf.sprintf "bin/results/%s/%s_%s%d_%d.csv" gen_name fv_name
      (List.fold_left (fun acc x -> acc ^ string_of_float x ^ "_") "" goal_list)
      !iterations !n_reset
  in

  (* Printf.printf "bin/results/%s/%s_%s%d_%d.csv" gen_name fv_name
     (List.fold_left (fun acc x -> acc ^ string_of_float x ^ "_") "" goal_list)
     !iterations !n_reset; *)
  let result_oc =
    if !print_one then top_oc else (* results_oc *)
                                open_out file_path
  in

  weights := Array.init n_weights (fun _ -> 500);

  (* run initial *)
  let start_time = Unix.gettimeofday () in
  let results = collect sample_size g in
  let end_time = Unix.gettimeofday () in

  let init_dist, init_score = f goal_list results in
  let init_weights = !weights in

  (* run with adjustment *)
  let use_neg_w =
    List.mem gen_name
      [
        "parametrized/sized_list_1";
        "parametrized/sized_list_2";
        "parametrized/depthtree_2";
        "parametrized/depthbst_2";
        "parametrized/rbtree_2";
      ]
  in
  let oc = open_out "bin/iterations.csv" in

  let fin_weights, fin_score, fin_dist, fin_time =
    match !search_strat_str with
    | "sa" ->
        random_restart oc g f goal_list !iterations use_neg_w
          simulated_annealing
    | "di" -> dumb_iterate oc g f goal_list true
    | "dir" | "dirs" | "dirw" -> dumb_iterate_ratios oc g f goal_list true
    | _ -> failwith "invalid search stragtegy"
  in

  close_out oc;

  (* let fin_weights = !weights in *)
  pp_res Format.std_formatter
    ( gen_name,
      fv_name,
      goal_list,
      !total_iterations,
      !total_restarts,
      init_dist,
      end_time -. start_time,
      init_weights,
      fin_dist,
      fin_time,
      fin_weights );

  print_csv result_oc
    ( gen_name,
      fv_name,
      goal_list,
      !total_iterations,
      !total_restarts,
      init_dist,
      end_time -. start_time,
      init_weights,
      init_score,
      fin_dist,
      fin_time,
      fin_weights,
      fin_score );
  ()

(* generators *)
let sortedlist_gen = ("frequency/sorted_list", sortedlist, 4, 0, 0)
let uniquelist_gen = ("frequency/unique_list", uniquelist, 0, 0, 0)
let sizedlist_gen = ("frequency/sized_list", sizedlist, 2, 1, 0)
let evenlist_gen = ("frequency/even_list", evenlist, 2, 1, 0)
let rbtree_gen = ("frequency/rb_tree", rbtree, 4, 2, 0)
let depthtree_gen = ("frequency/depth_tree", depthtree, 2, 1, 0)
let depthbst_gen = ("frequency/depth_bst", depthbst, 2, 1, 0)
let dragen_gen = ("Dragen", dragen_tree, 6, 2, 0)
let ld_rbtree_gen = ("LoadedDice", ld_rbtree, 5 * 8, 0, 0)

let sizedlist_para_enum_gen_5 =
  ("parametrized_enumeration/sized_list_5", sizedlist_para_enum, 12, 1, 0)

let sizedlist_para_enum_gen_10 =
  ("parametrized_enumeration/sized_list_10", sizedlist_para_enum, 22, 1, 0)

let sizedlist_para_1_gen =
  ("parametrized/sized_list_1_const", sizedlist_para_1, 3, 1, 0)

let sizedlist_para_2_gen = ("parametrized/sized_list", sizedlist_para_2, 4, 1, 0)
let evenlist_para_2_gen = ("parametrized/even_list", evenlist_para_2, 4, 1, 0)
let depthtree_para_2_gen = ("parametrized/depth_tree", depthtree_para_2, 4, 1, 0)
let depthbst_para_2_gen = ("parametrized/depth_bst", depthbst_para_2, 4, 1, 0)
let rbtree_para_2_gen = ("parametrized/rb_tree", rbtree_para_2, 8, 2, 0)
let ur_depthtree_gen = ("unrolled/depth_tree", depthtree_ur, 6, 1, 0)
let ur_depthbst_gen = ("unrolled/depth_bst", depthbst_ur, 6, 1, 0)
let ur_rbtree_gen = ("unrolled/rb_tree", rbtree_ur, 20, 2, 0)
let ur_sizedlist_gen = ("unrolled/sized_list", sizedlist_ur, 4, 1, 0)
let ur_evenlist_gen = ("unrolled/even_list", evenlist_ur, 4, 1, 0)

let ur_lin_depthtree_gen =
  ("unrolled_linear/depth_tree", depthtree_ur_lin, 12, 1, 0)

let ur_lin_depthbst_gen =
  ("unrolled_linear/depth_bst", depthbst_ur_lin, 12, 1, 0)

(* feature vectors *)
let nil_list_fv = ("nil", get_exact_score nil_fv)
let min_nil_list_fv = ("min_nil", get_score nil_fv)
let len_list_fv = ("len", get_exact_score len_fv)
let min_len_list_fv = ("min_len", get_score len_fv)
let nil_list_fv_2 = ("nil2", get_exact_score2 nil_fv)
let min_nil_list_fv_2 = ("min_nil2", get_score2 nil_fv)
let len_list_fv_2 = ("len2", get_exact_score2 len_fv)
let min_len_list_fv_2 = ("min_len2", get_score2 len_fv)
let bail_list_fv = ("bailouts", get_score bailout_fv)
let b_rbtree_fv = ("black", get_exact_score b_fv)
let min_b_rbtree_fv = ("min_black", get_score b_fv)
let height_tree_fv = ("height", get_exact_score height_fv)
let min_height_tree_fv = ("min_height", get_score height_fv)
let stick_tree_fv = ("stick", get_exact_score stick_fv)
let min_stick_tree_fv = ("min_stick", get_score stick_fv)
let h_balanced_tree_fv = ("h_bal", get_exact_score h_balanced_fv)
let count_cons = ("constructors", get_chi_score count_constr_list)
let uni_len_list_fv = ("uni_len", get_uniform_score length_acc)
let uni_height_rbtree_fv = ("uni_height", get_uniform_score height_rbt_acc)
let uni_height_tree_fv = ("uni_height", get_uniform_score height_tree_acc)

(* (fv, goal) *)
let (sizedlist_tests :
      ((string * (float list -> 'a list list -> (float * float list) * float))
      * float list)
      list) =
  [
    (nil_list_fv, [ 0.1 ]);
    (* (min_nil_list_fv, [ 0.1 ]); *)
    (len_list_fv, [ 5. ]);
    (* (min_len_list_fv, [ 5. ]); *)
    (uni_len_list_fv, [ 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10. ]);
  ]

let sizedlist_tests_2 =
  [
    (nil_list_fv_2, [ 0.1 ]);
    (* (min_nil_list_fv_2, [ 0.1 ]); *)
    (len_list_fv_2, [ 5. ]);
    (* (min_len_list_fv_2, [ 5. ]); *)
  ]

let sortedlist_tests = [ (bail_list_fv, [ 0.01 ]) ]

let evenlist_tests =
  [
    (nil_list_fv, [ 0.1 ]);
    (* (min_nil_list_fv, [ 0.1 ]); *)
    (len_list_fv, [ 5. ]);
    (* (min_len_list_fv, [ 5. ]); *)
    (uni_len_list_fv, [ 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.; 11. ]);
  ]

let rbtree_tests =
  [
    (uni_height_rbtree_fv, [ 2.; 3.; 4. ]);
    (b_rbtree_fv, [ 0.2 ]);
    (b_rbtree_fv, [ 0.4 ]);
    (b_rbtree_fv, [ 0.333 ]);
    (b_rbtree_fv, [ 0.6 ]);
    (* (min_b_rbtree_fv, [ 0.2 ]); *)
  ]

let depthtree_tests =
  [
    (height_tree_fv, [ 3. ]);
    (h_balanced_tree_fv, [ 1.5 ]);
    (h_balanced_tree_fv, [ 0.3 ]);
    (stick_tree_fv, [ 0.8 ]);
    (stick_tree_fv, [ 0.5 ]);
    (stick_tree_fv, [ 0.1 ]);
    (* (min_height_tree_fv, [ 3. ]); *)
    (* (min_stick_tree_fv, [ 0.1 ]); *)
    (uni_height_tree_fv, [ 0.; 1.; 2.; 3.; 4.; 5. ]);
    (h_balanced_tree_fv, [ 2. ]);
  ]

let depthbst_tests =
  [
    (height_tree_fv, [ 5. ]);
    (h_balanced_tree_fv, [ 0.3 ]);
    (h_balanced_tree_fv, [ 2. ]);
    (stick_tree_fv, [ 0.8 ]);
    (stick_tree_fv, [ 0.5 ]);
    (stick_tree_fv, [ 0.1 ]);
    (* (min_height_tree_fv, [ 3. ]); *)
    (* (min_stick_tree_fv, [ 0.1 ]); *)
    (uni_height_tree_fv, [ 0.; 1.; 2.; 3.; 4.; 5. ]);
  ]

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

let para_enum_10_sizedlist_tests = [ (uni_len_list_fv, [ 10. ]) ]
let para_enum_5_sizedlist_tests = [ (uni_len_list_fv, [ 5. ]) ]
let p_sizedlist_tests = [ (uni_len_list_fv, [ 10. ]) ]
let rq3_sizedlist_tests = [ (uni_len_list_fv, [ 10. ]) ]
let rq3_depthtree_tests = [ (uni_height_tree_fv, [ 0.; 1.; 2.; 3.; 4.; 5. ]) ]

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
  | Tree_type of
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
    ("sized_list_2", List_type (sizedlist_gen, sizedlist_tests_2));
    ("sized_list", List_type (sizedlist_gen, sizedlist_tests));
    ("even_list", List_type (evenlist_gen, evenlist_tests));
    ("rb_tree", Rb_type (rbtree_gen, rbtree_tests));
    ("depth_tree", Tree_type (depthtree_gen, depthtree_tests));
    ("depth_bst", Tree_type (depthbst_gen, depthbst_tests));
    ("dragen", Dragen_type (dragen_gen, dragen_tests));
    ("loaded_dice", Rb_type (ld_rbtree_gen, rbtree_tests));
    ("sorted_list", List_opt_type (sortedlist_gen, sortedlist_tests));
    ( "pe_sized_list_5",
      List_type (sizedlist_para_enum_gen_5, para_enum_5_sizedlist_tests) );
    ( "pe_sized_list_10",
      List_type (sizedlist_para_enum_gen_10, para_enum_10_sizedlist_tests) );
    ("p1_sized_list", List_type (sizedlist_para_1_gen, sizedlist_tests));
    ("p2_sized_list", List_type (sizedlist_para_2_gen, sizedlist_tests));
    ("p2_even_list", List_type (evenlist_para_2_gen, evenlist_tests));
    ("p2_depth_tree", Tree_type (depthtree_para_2_gen, depthtree_tests));
    ("p2_depth_bst", Tree_type (depthbst_para_2_gen, depthbst_tests));
    ("p2_rb_tree", Rb_type (rbtree_para_2_gen, rbtree_tests));
    ("ur_depth_tree", Tree_type (ur_depthtree_gen, depthtree_tests));
    ("ur_depth_bst", Tree_type (ur_depthbst_gen, depthbst_tests));
    ("ur_rb_tree", Rb_type (ur_rbtree_gen, rbtree_tests));
    ("ur_sized_list", List_type (ur_sizedlist_gen, sizedlist_tests));
    ("ur_even_list", List_type (ur_evenlist_gen, evenlist_tests));
    ("ur_lin_depth_tree", Tree_type (ur_lin_depthtree_gen, depthtree_tests));
    ("ur_lin_depth_bst", Tree_type (ur_depthbst_gen, depthbst_tests));
    ("rq3_p2_sized_list", List_type (sizedlist_para_2_gen, rq3_sizedlist_tests));
    (* ("rq3_ur_depth_tree", Tree_type (ur_depthtree_gen, rq3_depthtree_tests)); *)
  ]

let evaluate_test test_list =
  match test_list with
  | List_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | List_opt_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | Rb_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | Tree_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl
  | Dragen_type (g, fvl) ->
      List.iter
        (fun x ->
          let fv, goal = x in
          evaluate g fv goal)
        fvl

let () =
  Arg.parse speclist set_data_type usage_msg;

  let test =
    match List.assoc_opt !data_type tests with
    | Some s -> s
    | None -> failwith "unknown test"
  in

  let _ = evaluate_test test in
  close_out top_oc;
  ()

(* Kolmogorov–Smirnov test / make buckets *)
