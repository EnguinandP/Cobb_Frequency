module Env = Zzenv
open Frequency_combinators

let cur_temp = 0.01 

(* difference of distr from goal *)
let calc_score results goal feature_vector = 
  let pass = List.fold_left (fun acc x ->if feature_vector x then acc +. 1. else acc) 0. results in
  let dist = pass /. float_of_int (List.length results) in
  (dist, dist -. goal)

(* steps distribution .1 *)
let step cand_weight direction =
  if direction then
    !weights.(0) <- cand_weight.(0) - 5
  else
    !weights.(0) <- cand_weight.(0) + 5;
  !weights
    

let time_out_ref = ref false

exception Timed_out

let () =
  Core.Signal.Expert.handle Core.Signal.alrm (fun (_ : Core.Signal.t) ->
      Printf.printf "Timed out";
      time_out_ref := true)

let run_X_times (output: string) (goal : float) (f : unit -> 'a) (feature_vector : ('a -> bool))
    (num : int) =

  let results = [] in
  let start_time = Unix.gettimeofday () in

  let result_oc = open_out "bin/results.result" in
  Printf.fprintf result_oc "weights, score, distribution, time\n";

  (* curr_weight - best
    cand_weight - in trial *)
  let rec loop n curr_weight best_score =
    if n = num  || best_score <= 0. then
      best_score
    else
      (* collecting results *)
      let rec collect n results =
        if !time_out_ref then raise Timed_out;
        if n = 1000 then results
        else
          let gen_value = f () in
          (* let oc = open_out output in
          List.iter (Printf.fprintf oc "%d, ") gen_value;
          Printf.fprintf oc "\n"; 
          close_out oc;
          *)
        
          (* collects generated values in results *)
          let results = gen_value :: results in
          collect (n + 1) results

      in
      let results = collect 0 results in
      let end_time : float = Unix.gettimeofday () in

      (* calculates score *)
      let distr, score = calc_score results goal feature_vector in

      Printf.fprintf result_oc "(%d, %d), (%d, %d), %f, %f, %f\n" curr_weight.(0) curr_weight.(1) !weights.(0) !weights.(1) score distr (end_time -. start_time);
      Printf.printf "(%d, %d), (%d, %d), %f, %f, %f\n" curr_weight.(0) curr_weight.(1) !weights.(0) !weights.(1) score distr (end_time -. start_time);

      (* create new weight *)
      (* udpate weights *)

      if score < best_score then
        (* weights work better *)
        (* keep weight just tested *)
        let w0 = !weights.(0) in
        let w1 = !weights.(1) in
        let _ = step !weights true in
        Printf.printf "%d - curr: %d:%d  next: %d:%d\n\n" n w0 w1 !weights.(0) !weights.(1);
        loop (n + 1) [|w0; w1|] score
      else
        (* generate new weights and *)
        let _ = step curr_weight false in
        Printf.printf "%d - curr: %d:%d  next: %d:%d\n\n" n curr_weight.(0) curr_weight.(1) !weights.(0) !weights.(1);
        loop (n + 1) curr_weight best_score
  in

  let best_score = loop 0 !weights 1. in
  close_out result_oc;

  best_score



let () = QCheck_runner.set_seed 42

let () = 
  let filename = "bin/gen_values.result" in
  let goal = 0.1 in
  let _ = 
    run_X_times 
      filename
      goal
      (fun () -> Generators.Sizedlist_trans.sized_list_gen 10)
      (fun l-> l = [])
      20
  in 
  Printf.printf "solution: %d %d\n" !weights.(0) !weights.(1);

