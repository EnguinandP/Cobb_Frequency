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
    ("-i", Arg.Int (fun s -> iterations := s), "Set iterations");
    ("-r", Arg.Int (fun s -> n_reset := s), "Set random restarts");
    ( "-f",
      Arg.String (fun s -> feature_vector := s),
      "run with specified feature vector" );
    ("-one", Arg.Set print_one, "print one in one file");
    ( "-s",
      Arg.String (fun s -> search_strat_str := s),
      "Set search strategy: di, dir, sa" );
  ]

(** collects n values with gen *)
let collect n gen =
  List.init n (fun _ -> gen ())

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

let exit_cond dist goal = dist <= goal

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

  let rec restart n niter best_res (* : (int array * float * float * 'a list) list *)
      =
    let length = List.length best_res in
    let score = match best_res with
    | (_, s, _, _, _) :: _ :: _ :: _ :: _ :: [] when s <= 0. -> s
    | _ -> Float.max_float in

    (* Printf.printf "s = %f\n" score; *)

    if n >= !n_reset || (score <= 0.) then 
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
        List.sort (fun (_, s1, _, _, _) (_, s2, _, _, _) -> compare s2 s1) best_res
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

