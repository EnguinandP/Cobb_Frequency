module Env = Zzenv
open Frequency_combinators

let cur_temp = 0.01 

(* difference of distr from goal *)
let calc_score results goal feature_vector = 
  let pass = List.fold_left (fun acc x ->if feature_vector x then acc +. 1. else acc) 0. results in
  let dist = pass /. float_of_int (List.length results) in
  (dist, dist -. goal)

(* steps distribution .1 *)
let step score best_score =
  if score < best_score || Random.float 1.0 < cur_temp then
    !weights.(0) <- !weights.(0) + 1
  else 
    Printf.printf "%f %f\n" score best_score;
    !weights.(0) <- !weights.(0) - 1
    

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

  let rec loop n best_score =
    if n = num  || best_score <= goal then
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

      let weights = !weights in
      Printf.fprintf result_oc "(%d, %d), %f, %f, %f\n" weights.(0) weights.(1) score distr (end_time -. start_time);

      (* udpate weights *)
      let _ = step score best_score in

      loop (n + 1) score
  in

  let best_score = loop 0 1. in
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
      2000
  in 
  Printf.printf "solution: %d %d\n" !weights.(0) !weights.(1);

