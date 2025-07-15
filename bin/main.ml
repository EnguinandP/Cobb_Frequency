module Env = Zzenv
open Frequency_combinators
open Stdlib

let init_temp = 5230. (* 300 *) 
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
balanceness - duplicate elements - input to sizedlist bounds - parent -child difference can't control
could control values at nodes different nat gens - number of elements/nodes in list/tree 
- pressures precondition to not fails
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

let rec collect n results f =
  (* if !time_out_ref then raise Timed_out; *)
  if n = sample_size then results
  else
    let gen_value = f () in
    (* let oc = open_out output in
    List.iter (Printf.fprintf oc "%d, ") gen_value;
    Printf.fprintf oc "\n"; 
    close_out oc;
    *)
  
    (* collects generated values in results *)
    let results = gen_value :: results in
    collect (n + 1) results f

(* score is % of nil lists *)
let calc_score results goal feature_vector = 
  let pass = List.fold_left (fun acc x -> if feature_vector x then acc +. 1. else acc) 0. results in
  let dist = pass /. float_of_int (List.length results) in
  (dist, dist -. goal)

(* difference of distr from goal *)
let calc_score_func results goal feature_vector = 
  (results, goal -. results)

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

  (* this is much harder to read *)
  let s =
    if Random.bool () then
      step
    else
      (* weight can't be negative *)
      if (step > cand_weight.(direction)) then
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

let simulated_annealing (result_oc: out_channel) (goal : float) (gen : unit -> 'a list) (*(gen : unit -> float )*) (feature_vector : ('a list -> bool)) 
    (niter : int) print_all =

  let temp = init_temp in

  (* let result_oc = if print_all then
    open_out output
  else
    open_out_gen [Open_creat; Open_text; Open_append] 0o666 output
  in *)

  let extra_oc = if print_all then Some result_oc else None in
  print_labels extra_oc;

  let rec loop n temp direction curr_weight curr_score best_weight best_score count spare =
      if n = niter then
        (best_weight, best_score)
      else 

      let _ = step_n_param curr_weight in

      let temp = update_temp n in
    
      (* collecting results *)
      let start_time = Unix.gettimeofday () in
      let results = collect 0 [] gen in

      let end_time : float = Unix.gettimeofday () in

      (* calculates score *)
      let cand_score, dist = calc_score results goal feature_vector in
      (* calculates score with noise *)
      (* let cand_score, spare, dist = calc_noisy_score spare results goal feature_vector in *)

      let _ = print_iterations result_oc (string_of_int n) curr_weight cand_score dist (end_time -. start_time) in


      (* when score is worse, e^+ -> true *)
      if Random.float 1.0 < Float.exp (-. (cand_score -. curr_score) /. temp) then
        (* keep weight just tested *)
        (* let _ = Printf.fprintf result_oc "ACCEPT " in *)
        let w = Array.map (fun r -> r) !weights in
        if (cand_score < best_score) then
          loop (n + 1) temp direction w cand_score w cand_score count spare
        else
          loop (n + 1) temp direction w cand_score best_weight best_score count spare
      else
        (* let _ = Printf.fprintf result_oc "REJECT " in *)
        loop (n + 1) temp (not direction) curr_weight curr_score best_weight best_score (count +1) spare
  in

  let (best_weight, best_score) = loop 0 temp true !weights 1. !weights (1000.) 0 None in
  let _ = print_solutions extra_oc best_weight best_score in
  (* let _ = if print_all then
    close_out result_oc
  else
    () in *)

  (best_weight, best_score)

let basin_hoppping (output: string) (goal : float) (gen) (feature_vector : ('a -> bool)) (niter : int) print_all =
  (* initial solution is stored in the ref *)

  let result_oc = if print_all then
    open_out output
  else
    open_out_gen [Open_creat; Open_text; Open_append] 0o666 output 
  in

  let extra_oc = if print_all then Some result_oc else None in
  print_labels extra_oc;

  (* minimize is local min search *)
  let rec minimize best_weight best_score n spare = (

    (* next step *)
    let _ = step best_weight in
    (* let results = collect 0 [] gen in
    let next_score, dist = calc_score results goal feature_vector in *)
    let results = gen () in
    let next_score, spare, dist = calc_noisy_score spare results goal feature_vector in

    if n > 100 then 
      (best_weight, best_score)
    else
      (* if score is closer to min, then take step *)
      let w = Array.map (fun r -> r) !weights in
      if next_score < best_score then
        minimize w next_score (n + 1) spare
      else
        minimize best_weight best_score (n + 1) spare
  ) in

  let rec loop curr_weight curr_min best_weight best_score count spare =
    if count > niter then
      (best_weight, best_score)
    else 
      let temp = update_temp count in
      (* perturbation (step) *)
      let _ = step_in_range curr_weight (30,50) in  (* try adaptive stepwise *)

      let start_time = Unix.gettimeofday () in
      let results = gen () in
      (* let results = collect 0 [] gen in *)
      let end_time : float = Unix.gettimeofday () in
      (* let _, dist = calc_score results goal feature_vector in *)
      let _, spare, dist = calc_noisy_score spare results goal feature_vector in


      (* minimize *)
      let next_min_weight, next_min = minimize (Array.map (fun r -> r) !weights) 100000. 0 spare in
      (* Printf.printf "%d %f %f\n" !weights1.(0) next_score next_min; *)

      let _ = print_iterations result_oc (string_of_int count) next_min_weight next_min dist (end_time -. start_time) in
      (* Printf.printf "%d,%d,%d,%f,%f,0,0\n" count curr_weight.(0) !weights1.(0) next_min dist; *)

      (* acceptance test *)
      if Random.float 1.0 < Float.exp (-. (next_min -. curr_min) /. temp) then
        (* let w = Array.map (fun r -> r) !weights in *)
        if (next_min < best_score) then
          loop next_min_weight next_min next_min_weight next_min (count + 1) spare
        else 
          loop next_min_weight next_min best_weight best_score (count + 1) spare
      else
        loop curr_weight curr_min best_weight best_score (count + 1) spare
    in

  (* min is intial solution *)
  let init_weight, init_min = minimize (Array.map (fun r -> r) !weights) 100000. 0 None in
  let best_weights, best_score = loop init_weight init_min init_weight init_min 0 None in
  let _ = print_solutions extra_oc best_weights best_score in

  let _ = if print_all then
    close_out result_oc
  else
    () in
     (* close_out result_oc; *)

  best_weights, best_score

let random_restart (result_oc: out_channel) (goal : float) (gen : unit -> 'a list) (feature_vector : ('a list -> bool)) (niter : int) algor =
  let restart_interval = niter / 5 in

  (* let result_oc = open_out output in *)
  print_labels (Some result_oc);

  let rec restart n best_weight best_score =
    (* restarts 5 times *)
    if n >= 5 then
      (best_weight, best_score)
    else
        (* new location between -1000 and 1000 *)
        let new_start = Array.map (fun _ -> Random.int 2000 - 1000) !weights in
        weights := new_start;
        (* Printf.printf "\n%d\n" !weights.(0); *)
        let (weight, score) = algor result_oc 0. gen feature_vector restart_interval false in

      if score < best_score then
        restart (n + 1) weight score
      else
        restart (n + 1) best_weight best_score

      in

  let (best_weight, best_score) = restart 0 !weights 100000. in

  let _ = print_iterations result_oc "solution" best_weight best_score 0. 0. in
  (* close_out result_oc; *)
  (best_weight, best_score)



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

(* sizedlist generator with size 10 *)
let sizedlist () = Generators.Sizedlist_trans.sized_list_gen 10

let () = QCheck_runner.set_seed 42

let () = 

  (* let l = sizedlist () in
  print_int (List.length l);
  print_newline ();
  List.iter (Printf.printf "%d, ") l;
  Printf.printf "\n";  *)

  let filename = "bin/results.result" in
  let oc = open_out filename in 
  let (w, s) = 
    random_restart
      oc
      0.
      (sizedlist)
      (fun l -> l = [])
      10
      simulated_annealing  
  in 
  close_out oc;
  (* Printf.printf "solution: (%d, %d) %f\n" w.(0) w.(1) s; *)
  Printf.printf "solution: %f (" s;
  Array.iter (fun x -> Printf.printf "%d," x) w;
  Printf.printf ")\n";




