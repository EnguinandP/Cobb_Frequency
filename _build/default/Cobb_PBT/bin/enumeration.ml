open Combinators

let timeout = 300 (*  5 minutes *)
let time_out_ref = ref false

exception Timed_out

let () =
  Core.Signal.Expert.handle Core.Signal.alrm (fun (_ : Core.Signal.t) ->
      Printf.printf "Timed out";
      time_out_ref := true)

let run_X_times (f : unit -> 'a) (check_opt : ('a -> bool) option)
    (count : 'a -> int) (num : int) : float =
  let sum = ref 0 in
  let start_time = Unix.gettimeofday () in

  let rec loop n =
    if !time_out_ref then raise Timed_out;
    if n = num then ()
    else
      let result = f () in
      match check_opt with
      | Some check ->
          if check result then (
            let instance_count = count result in
            sum := !sum + instance_count;
            loop (n + 1))
          else loop n
      | None ->
          let instance_count = count result in
          sum := !sum + instance_count;
          loop (n + 1)
  in
  let () = loop 0 in
  let end_time : float = Unix.gettimeofday () in
  print_int !sum;
  print_newline ();
  end_time -. start_time

let () = QCheck_runner.set_seed 42

(* let f : float = run_X_times (fun () : int -> int_gen ()) (Some (fun (_: int) -> true)) (fun (x: int) -> x) 10000
let () = Printf.printf "Time: %f\n" f
let f : float = run_X_times (fun () : int -> int_gen ()) (Some (fun (_: int) -> true)) (fun (x: int) -> x) 100000
let () = Printf.printf "Time: %f\n" f *)

open Rbtree

(* Note that for both of these, what we are calculating for is Black height, this
is twice the normal height of the tree *)
let rbtree_gen h =
  let color = true in
  let inv = if color then h + h else h + h + 1 in
  Prog.rbtree_gen inv color h

let default h = gen_sized_int_root_red_rbtree (2 * h)
let black_heights : int list = [ 1; 2; 3; 4; 5; 6 ]
let number_of_trees : int list = [ 1000; 10000 ]

let synthesized_gen_results_file =
  "bin/enumeration_data/synthesis_results.csv.results"

let default_results_file = "bin/enumeration_data/default_results.csv.results"

let benchmarks =
  [
    (synthesized_gen_results_file, rbtree_gen); (default_results_file, default);
  ]

(* let rbtree_prog_time : float = run_X_times (fun () -> rbtree_gen 5) None (count_rbtree_nodes)  100000
let () = Printf.printf "Time: %f\n" rbtree_prog_time

let rbtree_prog_time : float = run_X_times (fun () -> (rbtree_gen 5, 5)) (Some (fun (t, h) -> rbtree_invariant t h)) (fun x -> count_rbtree_nodes (fst x))  100000
let () = Printf.printf "Time: %f\n" rbtree_prog_time *)

let () =
  List.iter
    (fun (filename, gen) ->
      let oc = open_out filename in

      let () = output_string oc "Black height,Num generated,Time\n" in

      (* let () = Core.Out_channel.write_all
  "bin/enumeration_data/luck_results.csv.results"
  ~data:(Printf.sprintf "Black height,Num generated,Time\n") *)

      (*  let rbtree_prog_time : float = run_X_times (fun () -> rbtree_gen 5) None (count_rbtree_nodes)  100000 *)
      let () =
        List.iter
          (fun h ->
            List.iter
              (fun n ->
                (* This will also cancel any previously set alarm *)
                ignore (Core_unix.alarm timeout);
                time_out_ref := false;

                try
                  if gen == default then print_endline "hello";
                  let rbtree_prog_time : float =
                    run_X_times
                      (fun () -> (gen h, h))
                      (Some (fun (t, h) -> Precondition.rbtree_invariant t h))
                      (fun (x, _) -> count_rbtree_nodes x)
                      n
                  in
                  output_string oc
                    (Printf.sprintf "%d,%d,%f\n" h n rbtree_prog_time)
                with Timed_out -> print_endline "Caught timed out")
              number_of_trees)
          black_heights
      in

      close_out oc)
    benchmarks
