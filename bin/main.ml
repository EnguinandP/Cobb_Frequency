module Env = Zzenv
open Frequency_combinators
open Stdlib

let init_temp = 500. (* 300 *)
let sample_size = 1000
let step_size = 1
let step_range = (1, 20)
let reset = 300

(* hyperparameter *)
(* add best candidate + best score *)
(* pool of cand , test each one and take next step*)
(* list of best and final evaluation *)
(* reorder loop to propose eval decide *)
(* take test gen out and use functions with weird local min and max *)
(* branching process for quickcheck genrators Augustin Mista *)

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

(* steps 10 works with sample size 10000 *)
let step10 cand_weight direction =
  if direction then
    !weights.(0) <- cand_weight.(0) + 10
  else
    !weights.(0) <- cand_weight.(0) - 10;
  !weights

(* steps 50 works with sample size 1000 *)
let step50 cand_weight direction =
  if direction then
    !weights.(0) <- cand_weight.(0) + 50
  else
    !weights.(0) <- cand_weight.(0) - 50;
  !weights

(* steps 50 up and down works with sample size 1000 *)
let stepAll50 cand_weight direction =
  if direction then begin
    !weights.(0) <- cand_weight.(0) + 50;
    !weights.(1) <- cand_weight.(1) - 50
  end else begin
    !weights.(0) <- cand_weight.(0) - 50;
    !weights.(1) <- cand_weight.(1) + 50
  end;
  !weights

let step_f1 cand_weight direction =
  if direction && Random.bool () then
    weights_f1 := cand_weight + step_size
  else
    weights_f1 := cand_weight - step_size;
  !weights_f1

let step cand_weight direction =
  let step = Random.int (snd step_range) + fst step_range in
  if Random.bool () then
    weights_f1 := cand_weight + step
  else
    weights_f1 := cand_weight - step;
  !weights_f1

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



(* updates the temperature *)
let update_temp n = init_temp /. (float_of_int n +. 1.)

let time_out_ref = ref false

(* exception Timed_out *)

let () =
  Core.Signal.Expert.handle Core.Signal.alrm (fun (_ : Core.Signal.t) ->
      Printf.printf "Timed out";
      time_out_ref := true)

let run_X_times (output: string) (goal : float) (gen) (feature_vector : ('a -> bool))
    (num : int) =

  let temp = init_temp in
  let start_time = Unix.gettimeofday () in

  let result_oc = open_out "bin/results.result" in
  Printf.fprintf result_oc "iteration,curr weight x,curr weight y,cand weights x,cand weights y,score,distribution,time,temp\n";

  let rec loop n temp direction curr_weight curr_score best_weight best_score count =
      if n = num then
        (best_weight, best_score)
      else 
        
      let count, curr_score = if count >= 300 then
        (let curr_weight = best_weight in
        let _ = step_2_param curr_weight in
        0, best_score)
      else
      let _ = step_2_param curr_weight in
        count, curr_score 
      in
      
      (* let _ = step_2_param curr_weight in *)
      (* Printf.printf "%d - curr: %d   next:%d\n\n" n curr_weight !weights_f1; *)
      let temp = update_temp n in
    
      (* collecting results *)
      let results = gen () in
      let end_time : float = Unix.gettimeofday () in

      (* calculates score *)
      let cand_score, dist = calc_score results goal feature_vector in

      Printf.fprintf result_oc "%d,%d,%d,%d,%d,%f,%f,%f,%f\n" n curr_weight.(0) curr_weight.(1) !weights.(0) !weights.(1) cand_score dist (end_time -. start_time) (cand_score -. curr_score);
      (* Printf.fprintf result_oc "%d,%d,%d,%f,%f,%f,%f\n" n curr_weight !weights_f1 cand_score dist (end_time -. start_time) (cand_score -. curr_score); *)
      (* Printf.printf "%d, %d, %f, %f, %f\n" curr_weight !weights_f1 score dist (end_time -. start_time); *)

      (* when score is worse, e^+ -> true *)
      if Random.float 1.0 < Float.exp (-. (cand_score -. curr_score) /. temp) then
        (* keep weight just tested *)
        (* let _ = print_endline "ACCEPT cand" in *)
        (* let _ = Printf.fprintf result_oc "ACCEPT " in *)
        if (cand_score < best_score) then
          loop (n + 1) temp direction [|!weights.(0); !weights.(1)|] cand_score [|!weights.(0); !weights.(1)|] cand_score count
        else
          loop (n + 1) temp direction [|!weights.(0); !weights.(1)|] cand_score best_weight best_score count
      else
        (* let _ = print_endline "REJECT cand" in *)
        (* let _ = Printf.fprintf result_oc "REJECT " in *)
        loop (n + 1) temp (not direction) curr_weight curr_score best_weight best_score (count +1)
  in

  let (best_weight, best_score) = loop 0 temp true !weights 1. !weights (1000.) 0 in

  Printf.fprintf result_oc "solution,0,%d,%d,0,0,%f,0,0\n" best_weight.(0) best_weight.(1) best_score;
  (* Printf.fprintf result_oc "solution,0,%d,%f,0,0\n" best_weight best_score; *)
  close_out result_oc;

  (best_weight, best_score)


let () = QCheck_runner.set_seed 42

(* 1 paramater functions *)
let f1 () = 
  let x = float_of_int(!weights_f1) in
    ( x /. 10.) *. (cos (x /. 25.))

let f2 () : float = 
  let x = float_of_int(!weights_f1) in
  (4. /. 30.) *. ( (x +. 50.)) *. sin ((x +. 50.) /. 20.) +. ((x -. 500.) /. 30.) *. ((x -. 500.) /. 30.)

let f3 () = 
  let x = float_of_int(!weights_f1) in
  if x = 0. then
    1. 
  else
    cos (50. *. Float.pi *. x /. 1000.) /. (x /. 1000.)

let f4 () = 
  let x = float_of_int(!weights_f1) in
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
 
let () = 
  let filename = "bin/gen_values.result" in
  let (w, s) = 
    run_X_times 
      filename
      0.
      (f6)
      (fun l-> l = [])
      2000
  in 
  Printf.printf "solution: (%d, %d) %f\n" w.(0) w.(1) s;

