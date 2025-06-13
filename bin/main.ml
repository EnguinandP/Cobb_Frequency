module Env = Zzenv
open Frequency_combinators
open Stdlib

let init_temp = 300. (* 300 *)
let sample_size = 1000
let step_size = 1
let step_range = (1, 20)
let reset = 300

(* meeting at 1 *)

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

(* difference of distr from goal *)
let calc_score results goal feature_vector = 
  (results, goal -. results)

(* difference of distr from goal *)
let calc_score_gen results goal feature_vector = 
  let pass = List.fold_left (fun acc x ->if feature_vector x then acc +. 1. else acc) 0. results in
  let dist = pass /. float_of_int (List.length results) in
  (dist, dist -. goal)


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
  

let is_nil l = l = []

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
    !weights.(direction) <- cand_weight.(direction) - step

let step cand_weight =
  let step = Random.int (snd step_range) + fst step_range in
  if Random.bool () then
    !weights.(0) <- cand_weight.(0) + step
  else
    !weights.(0) <- cand_weight.(0) - step;
  !weights


let step_2_param cand_weight =
  let step = Random.int (snd step_range) + fst step_range in

  if Random.bool () then
  (* changing x *)
    (if Random.bool () then
      !weights.(0) <- cand_weight.(0) + step
    else
      !weights.(0) <- cand_weight.(0) - step;)
  else
  (* changing y *)
    if Random.bool () then
      !weights.(1) <- cand_weight.(1) + step
    else
      !weights.(1) <- cand_weight.(1) - step;
  !weights

let step_n_param (cand_weight : int array) =
  let step = Random.int (snd step_range) + fst step_range in

  let n = Array.length cand_weight in
  let direction = Random.int n in
  
  if Random.bool () then 
    !weights.(direction) <- cand_weight.(direction) + step
  else
    !weights.(direction) <- cand_weight.(direction) - step
  

(* updates the temperature *)
let update_temp n = init_temp /. (float_of_int n +. 1.)

(* print function for results *)
let print_iterations oc n curr_weight cand_score dist time =
  Printf.fprintf oc "%s," n;
  Array.iter (fun x -> Printf.fprintf oc "%d," x) curr_weight;
  Array.iter (fun x -> Printf.fprintf oc "%d," x) !weights;
  let _ = Printf.fprintf oc "%f,%f,%f\n" cand_score dist time
  in ()

let print_labels oc =
  Printf.fprintf oc "iteration,";
  Array.iteri (fun i x -> Printf.fprintf oc "curr weight %d," (i + 1)) !weights;
  Array.iteri (fun i x -> Printf.fprintf oc "cand weight %d," (i + 1)) !weights;
  let _ = Printf.fprintf oc "score,distribution,time\n"
  in ()

let time_out_ref = ref false

(* exception Timed_out *)

let () =
  Core.Signal.Expert.handle Core.Signal.alrm (fun (_ : Core.Signal.t) ->
      Printf.printf "Timed out";
      time_out_ref := true)

let simulated_annealing (output: string) (goal : float) (gen) (feature_vector : ('a -> bool))
    (niter : int) =

  let temp = init_temp in
  let start_time = Unix.gettimeofday () in

  let result_oc = open_out "bin/results.result" in
  print_labels result_oc;

  let rec loop n temp direction curr_weight curr_score best_weight best_score count =
      if n = niter then
        (best_weight, best_score)
      else 
        
      (* let count, curr_score = if count >= 300 then
        (let curr_weight = best_weight in
        let _ = step_2_param curr_weight in
        0, best_score)
      else *)
      let _ = step_n_param curr_weight in
        (* count, curr_score 
      in *)
      
      (* let _ = step_2_param curr_weight in *)
      (* Printf.printf "%d - curr: %d   next:%d\n\n" n curr_weight !weights_f1; *)
      let temp = update_temp n in
    
      (* collecting results *)
      let results = gen () in
      let end_time : float = Unix.gettimeofday () in

      (* calculates score *)
      let cand_score, dist = calc_score results goal feature_vector in

      let _ = print_iterations result_oc (string_of_int n) curr_weight cand_score dist (end_time -. start_time) in
      (* Printf.fprintf result_oc "%d,%d,%d,%d,%d,%f,%f,%f,%f\n" n curr_weight.(0) curr_weight.(1) !weights.(0) !weights.(1) cand_score dist (end_time -. start_time) (cand_score -. curr_score); *)
      (* Printf.fprintf result_oc "%d,%d,%d,%f,%f,%f,%f\n" n curr_weight !weights_f1 cand_score dist (end_time -. start_time) (cand_score -. curr_score); *)
      (* Printf.printf "%d, %d, %f, %f, %f\n" curr_weight !weights_f1 score dist (end_time -. start_time); *)

      (* when score is worse, e^+ -> true *)
      if Random.float 1.0 < Float.exp (-. (cand_score -. curr_score) /. temp) then
        (* keep weight just tested *)
        (* let _ = print_endline "ACCEPT cand" in *)
        (* let _ = Printf.fprintf result_oc "ACCEPT " in *)
        let w = Array.map (fun r -> r) !weights in
        if (cand_score < best_score) then
          loop (n + 1) temp direction !weights cand_score w cand_score count
        else
          loop (n + 1) temp direction !weights cand_score best_weight best_score count
      else
        (* let _ = print_endline "REJECT cand" in *)
        (* let _ = Printf.fprintf result_oc "REJECT " in *)
        loop (n + 1) temp (not direction) curr_weight curr_score best_weight best_score (count +1)
  in

  let (best_weight, best_score) = loop 0 temp true !weights 1. !weights (1000.) 0 in
  let _ = print_iterations result_oc "solution" best_weight best_score 0. 0. in
  (* Printf.fprintf result_oc "solution,0,%d,%d,0,0,%f,0,0\n" best_weight.(0) best_weight.(1) best_score; *)
  (* Printf.fprintf result_oc "solution,0,%d,%f,0,0\n" best_weight best_score; *)
  close_out result_oc;

  (best_weight, best_score)

let random_restart (output: string) (goal : float) (gen) (feature_vector : ('a -> bool)) (niter : int) =
  let restart_interval = niter / 5 in

  let temp = init_temp in
  let start_time = Unix.gettimeofday () in

  let result_oc = open_out "bin/results.result" in
  print_labels result_oc;

  let rec loop n temp direction curr_weight curr_score best_weight best_score count =
      if n = restart_interval then
        (best_weight, best_score)
      else 
        
      let _ = step_n_param curr_weight in

      let temp = update_temp n in
    
      (* collecting results *)
      let results = gen () in
      let end_time : float = Unix.gettimeofday () in

      (* calculates score *)
      let cand_score, dist = calc_score results goal feature_vector in
      let _ = print_iterations result_oc (string_of_int n) curr_weight cand_score dist (end_time -. start_time) in
      (* Printf.fprintf result_oc "%d,%d,%d,%d,%d,%f,%f,%f\n" n curr_weight.(0) curr_weight.(1) !weights.(0) !weights.(1) cand_score dist (end_time -. start_time); *)
      (* Printf.fprintf result_oc "%d,%d,%d,%f,%f,%f,%f\n" n curr_weight !weights_f1 cand_score dist (end_time -. start_time) (cand_score -. curr_score); *)

      (* when score is worse, e^+ -> true *)
      if Random.float 1.0 < Float.exp (-. (cand_score -. curr_score) /. temp) then
        (* keep weight just tested *)
        let w = Array.map (fun r -> r) !weights in
        if (cand_score < best_score) then
          loop (n + 1) temp direction w cand_score w cand_score count
        else
          loop (n + 1) temp direction w cand_score best_weight best_score count
      else
        loop (n + 1) temp (not direction) curr_weight curr_score best_weight best_score (count +1)
  in

  let rec restart n best_weight best_score =
    if n > 5 then
      (best_weight, best_score)
    else
        (* new location between -1000 and 1000 *)
        let new_start = Array.map (fun _ -> Random.int 2000 - 1000) !weights in
        (* let new_start = [|Random.int 2000 - 1000; Random.int 2000 - 1000|] in *)
        let (weight, score) = loop 0 temp true new_start 1. new_start (100000.) 0 in

      if score < best_score then
        restart (n + 1) weight score
      else
        restart (n + 1) best_weight best_score

      in

  let (best_weight, best_score) = restart 0 !weights 100000. in

  let _ = print_iterations result_oc "solution" best_weight best_score 0. 0. in
  close_out result_oc;
  (best_weight, best_score)




let basin_hoppping (output: string) (goal : float) (gen) (feature_vector : ('a -> bool)) (niter : int) =
  (* initial solution is stored in the ref *)
  (* let init_weight = Array.map (fun r -> r) !weights in *)
  (* let results = gen () in *)
  (* let init_score, _ = calc_score results goal feature_vector in *)

  let result_oc = open_out "bin/results.result" in
  print_labels result_oc;

  (* minimize is local min search *)
  let rec minimize best_weight best_score n = (

    (* next step *)
    let _ = step best_weight in
    let results = gen () in
    let next_score, dist = calc_score results goal feature_vector in

    if n > 100 then 
      (best_weight, best_score)
    else
      (* if score is closer to min, then take step *)
      let w = Array.map (fun r -> r) !weights in
      if next_score < best_score then
        minimize w next_score (n + 1)
      else
        minimize best_weight best_score (n + 1)
  ) in

  let rec loop curr_weight curr_min best_weight best_score count =
    if count > niter then
      (best_weight, best_score)
    else 
      let temp = update_temp count in
      (* perturbation (step) *)
      let _ = step_in_range curr_weight (30,50) in  (* try adaptive stepwise *)

      let start_time = Unix.gettimeofday () in
      let results = gen () in
      let end_time : float = Unix.gettimeofday () in
      let _, dist = calc_score results goal feature_vector in

      (* minimize *)
      let next_min_weight, next_min = minimize (Array.map (fun r -> r) !weights) 100000. 0 in
      (* Printf.printf "%d %f %f\n" !weights1.(0) next_score next_min; *)

      let _ = print_iterations result_oc (string_of_int count) next_min_weight next_min dist (end_time -. start_time) in
      (* Printf.printf "%d,%d,%d,%f,%f,0,0\n" count curr_weight.(0) !weights1.(0) next_min dist; *)

      (* acceptance test *)
      if Random.float 1.0 < Float.exp (-. (next_min -. curr_min) /. temp) then
        (* let w = Array.map (fun r -> r) !weights in *)
        if (next_min < best_score) then
          loop next_min_weight next_min next_min_weight next_min (count + 1)
        else 
          loop next_min_weight next_min best_weight best_score (count + 1)
      else
        loop curr_weight curr_min best_weight best_score (count + 1)
    in

  (* min is intial solution *)
  let init_weight, init_min = minimize (Array.map (fun r -> r) !weights) 100000. 0 in
  let best_weights, best_score = loop init_weight init_min init_weight init_min 0 in
  let _ = print_iterations result_oc "solution" best_weights best_score 0. 0. in

  close_out result_oc;
  best_weights, best_score


let () = QCheck_runner.set_seed 42

(* 1 paramater functions *)
let f1 () = 
  let x = float_of_int(!weights.(0)) in
    ( x /. 10.) *. (cos (x /. 25.))

let f2 () : float = 
  let x = float_of_int(!weights.(0)) in
  (4. /. 30.) *. ( (x +. 50.)) *. sin ((x +. 50.) /. 20.) +. ((x -. 500.) /. 30.) *. ((x -. 500.) /. 30.)

let f3 () = 
  let x = float_of_int(!weights.(0)) in
  if x = 0. then
    1. 
  else
    cos (50. *. Float.pi *. x /. 1000.) /. (x /. 1000.)

let f4 () = 
  let x = float_of_int(!weights.(0)) in
  -500. *. (sin x *. 30.) /. (x *. 40.)

(* 2 parameter functions *)

(* cone *)
let f5 () = 
  let x = float_of_int(!weights.(0)) in
  let y = float_of_int(!weights.(1)) in
  100. *. (x /. 300.) *. (x /. 300.) +. (y /. 300.) *. (y /. 300.)

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

let () = 
  let filename = "bin/gen_values.result" in
  let (w, s) = 
    basin_hoppping 
      filename
      0.
      (f6)
      (fun l-> l = [])
      5000
  in 
  (* Printf.printf "solution: (%d, %d) %f\n" w.(0) w.(1) s; *)
  Printf.printf "solution: %f " s;
  Array.iter (fun x -> Printf.printf "%d," x) w;
  Printf.printf "\n";




