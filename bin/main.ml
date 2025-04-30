module Env = Zzenv

(* empty list <10% , no empty list 90% *)
(* calculates distribution *)
let get_distribution l = 
  let is_empty = List.filter (fun x -> x = []) l in
  let distr = float_of_int (List.length is_empty) /. float_of_int (List.length l) in
  distr

(* checks distribution *)
(* if distr is less than goal *)
let score_function_1 goal distr = 
  distr >= 0.1

(* difference of distr from goal *)
let score_function_2 goal distr = 
  goal -. distr

let time_out_ref = ref false

exception Timed_out

let () =
  Core.Signal.Expert.handle Core.Signal.alrm (fun (_ : Core.Signal.t) ->
      Printf.printf "Timed out";
      time_out_ref := true)

let run_X_times (output: string) (goal : float) (f : unit -> 'a) (check_opt : ('a -> bool) option)
    (num : int) : float =
  let count = ref 0.0 in
  let start_time = Unix.gettimeofday () in

  let oc = open_out output in

  let rec loop n =
    if !time_out_ref then raise Timed_out;
    if n = num then ()
    else
      let result = f () in
      List.iter (Printf.fprintf oc "%d, ") result;
      Printf.fprintf oc "\n";

      match check_opt with
      | Some check ->
          if [] == result then (
            count := !count +. 1.0;
            loop (n + 1))
          else loop (n + 1)
      | None ->
          loop (n + 1)
  in
  let () = loop 0 in
  let end_time : float = Unix.gettimeofday () in
  let distr = !count /. float_of_int num in
  let score_1 = score_function_1 goal distr in
  let score_2 = score_function_2 goal distr in

  let result_oc = open_out "bin/results.result" in
  Printf.fprintf result_oc "Collected %d outputs with goal distribution %f\n\n" num goal;
  Printf.fprintf result_oc "distribution: %f \nscore 1: %b \nscore 2: %f\n" distr score_1 score_2;
  print_newline ();
  end_time -. start_time

let () = QCheck_runner.set_seed 42

let () = 
  let filename = "bin/gen_values.result" in
  let goal = 0.1 in
  let prog_time = 
    run_X_times 
      filename
      goal
      (fun () -> Generators.Sizedlist_trans.sized_list_gen 10)
      (Some (fun l-> l = []))
      20000 
  in
  Printf.printf "prog_time: %f\n" prog_time;

