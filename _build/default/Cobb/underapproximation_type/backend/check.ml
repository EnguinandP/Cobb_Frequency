open Z3
open Solver
open Goal
module Env = Zzenv
(* open Z3aux *)

type smt_result = SmtSat of Model.model | SmtUnsat | Timeout

let solver_result solver =
  (* let _ = printf "solver_result\n" in *)
  match check solver [] with
  | UNSATISFIABLE -> SmtUnsat
  | UNKNOWN ->
      (* raise (InterExn "time out!") *)
      Printf.printf "\ttimeout\n";
      Timeout
  | SATISFIABLE -> (
      match Solver.get_model solver with
      | None -> failwith "never happen"
      | Some m -> SmtSat m)

let get_int m i =
  match Model.eval m i true with
  | None -> failwith "get_int"
  | Some v ->
      (* printf "get_int(%s)\n" (Expr.to_string i); *)
      int_of_string @@ Arithmetic.Integer.numeral_to_string v

let get_bool_str m i =
  match Model.eval m i true with None -> "none" | Some v -> Expr.to_string v

let get_int_name ctx m name =
  get_int m @@ Arithmetic.Integer.mk_const_s ctx name

let get_pred m predexpr =
  match Model.eval m predexpr true with
  | None -> failwith "get pred"
  | Some v -> Z3aux.z3expr_to_bool v

let get_unknown_fv ctx m unknown_fv =
  List.map (fun (_, b) -> get_pred m (Boolean.mk_const_s ctx b)) unknown_fv

let rlimit = ref 200000000
let optional_timeout = ref None

let smt_format_file filename solver =
  (match !optional_timeout with
  | Some x -> Printf.printf "Timeout: %d\n" x
  | None -> print_endline "No timeout");
  let oc = open_out filename in
  let prelude =
    "(set-option :rlimit " ^ string_of_int !rlimit ^ ")\n"
    ^ Option.fold ~none:""
        ~some:(fun x -> "(set-option :timeout " ^ string_of_int x ^ ")\n")
        !optional_timeout
  in
  let query = Z3.Solver.to_string solver in
  let postlude = "\n(check-sat)\n" in
  let postlude =
    if Core.String.is_substring ~substring:"double" filename then
      postlude ^ postlude
    else postlude
  in
  Printf.fprintf oc "%s%s%s" prelude query postlude;
  (* Printf.printf "%s%s%s" prelude query postlude; *)
  close_out oc

let run_first_to_decision commands =
  (* Start all processes *)
  let procs = List.map (fun cmd -> (cmd, Unix.open_process_in cmd)) commands in

  (* Get file descriptors with their associated process info *)
  let fd_procs =
    List.map
      (fun (cmd, proc) -> (Unix.descr_of_in_channel proc, cmd, proc))
      procs
  in

  (* Set all channels to non-blocking mode *)
  List.iter (fun (fd, _, _) -> Unix.set_nonblock fd) fd_procs;

  let cleanup () =
    List.iter (fun (_, _, proc) -> ignore (Unix.close_process_in proc)) fd_procs
  in

  let rec check_for_result remaining_fds =
    if remaining_fds = [] then "unknown"
    else
      (* Use select to wait for any ready file descriptor *)
      let ready_fds, _, _ =
        Unix.select (List.map (fun (fd, _, _) -> fd) remaining_fds) [] [] 0.01
      in

      if ready_fds = [] then check_for_result remaining_fds
      else
        let done_fds, remaining_fds =
          List.partition (fun (fd, _, _) -> List.mem fd ready_fds) remaining_fds
        in

        match
          List.find_map
            (fun (fd, cmd, proc) ->
              print_endline (cmd ^ " finished");
              Unix.clear_nonblock fd; (* TODO: Needing this clear is a hack and I think makes things weird?? *)
              (* TODO: Part of the problem is that this wakes up when the first
              line is done... but then we keep going *)
              let result = In_channel.input_all proc in

              print_endline ("Result: " ^ result);

              (* If result contains "unsat", return it immediately *)
              if Core.String.is_substring ~substring:"unsat" result then
                Some "unsat"
              else if Core.String.is_substring ~substring:"sat" result then
                Some "sat"
              else None)
            done_fds
        with
        | Some s -> s
        | None -> check_for_result remaining_fds
  in

  let res = check_for_result fd_procs in
  cleanup ();
  res

let run_both_commands cmd1 cmd2 =
  (* Start both processes *)
  let proc1 = Unix.open_process_in cmd1 in
  let proc2 = Unix.open_process_in cmd2 in

  (* Read first line from each process (or empty string if no output) *)
  let result1 = try input_line proc1 with End_of_file -> "" in

  let result2 = try input_line proc2 with End_of_file -> "" in

  (* Wait for both processes to complete *)
  ignore (Unix.close_process_in proc1);
  ignore (Unix.close_process_in proc2);

  print_endline "results";
  print_endline result1;
  print_endline result2;

  (* Return first line from each command and their exit statuses *)
  if result1 = "unsat" then result1 else result2

(* NGL, this is claude generated... we will see how it goes *)
let run_first_to_finish cmd1 cmd2 =
  (* Start both processes *)
  let proc1 = Unix.open_process_in cmd1 in
  let proc2 = Unix.open_process_in cmd2 in

  (* Get file descriptors *)
  let fd1 = Unix.descr_of_in_channel proc1 in
  let fd2 = Unix.descr_of_in_channel proc2 in

  (* Set both channels to non-blocking mode *)
  Unix.set_nonblock fd1;
  Unix.set_nonblock fd2;

  let rec check_for_result () =
    (* Use select to wait for any data *)
    let ready_read, _, _ = Unix.select [ fd1; fd2 ] [] [] 0.01 in

    let fd1_ready = List.mem fd1 ready_read in
    let fd2_ready = List.mem fd2 ready_read in

    (* Check if both commands have finished *)
    if fd1_ready && fd2_ready then (
      print_endline "Both commands finished";
      (* Both commands finished *)
      let result1 = input_line proc1 in
      let result2 = input_line proc2 in
      ignore (Unix.close_process_in proc1);
      ignore (Unix.close_process_in proc2);
      if result1 = "unsat" then result1 else result2)
    else if fd1_ready then (
      print_endline "First command finished";
      (* First command finished *)
      let result = input_line proc1 in
      ignore (Unix.close_process_in proc1);
      ignore (Unix.close_process_in proc2);
      result)
    else if fd2_ready then (
      print_endline "Second command finished";
      (* Second command finished *)
      let result = input_line proc2 in
      ignore (Unix.close_process_in proc1);
      ignore (Unix.close_process_in proc2);
      result)
    else
      (* Neither command has finished yet, keep checking *)
      check_for_result ()
  in

  try check_for_result () with
  | End_of_file ->
      (* Handle case where command finished but produced no output *)
      ignore (Unix.close_process_in proc1);
      ignore (Unix.close_process_in proc2);
      ""
  | e ->
      (* Clean up on exception *)
      ignore (Unix.close_process_in proc1);
      ignore (Unix.close_process_in proc2);
      raise e

let run_z3_in_process solver : smt_result =
  let filename = "subtyping_temp_file.smt2" in
  let filename2 = "subtyping_temp_file_double.smt2" in
  smt_format_file filename solver;
  let command = "z3 " ^ filename in
  let command2 = "z3 proof=true " ^ filename in

  smt_format_file filename2 solver;
  let command3 = "z3 proof=true " ^ filename2 in
  (* let status = Unix.system command in *)
  (*   let stdout, _std_else = Unix.open_process command in
  let status = input_line stdout in

  let _ = Unix.close_process (stdout, _std_else) in *)

  (* let status = run_first_to_finish command2 command in *)

  let status = run_first_to_decision [ command; command2; command3 ] in

  print_endline "----------------";
  print_endline status;
  print_endline "----------------";
  (*  match status with
  | WEXITED i -> Printf.printf "Exited with code: %d\n" i
  | WSIGNALED i -> Printf.printf "Killed by signal: %d\n" i
  | WSTOPPED i -> Printf.printf "Stopped by signal: %d\n" i *)
  if status = "unsat" (* status = WEXITED 0 *) then SmtUnsat else Timeout

let smt_solve ctx assertions =
  (* let _ = printf "check\n" in *)
  let solver = mk_solver ctx None in
  let g = mk_goal ctx true false false in
  let _ = Goal.add g assertions in

  (* let g = Goal.simplify g None in *)
  (* let g = *)
  (*   Tactic.(ApplyResult.get_subgoal (apply (mk_tactic ctx "snf") g None) 0) *)
  (* in *)
  (* let () = *)
  (*   Printf.printf "Goal: %s\n\n" *)
  (*   @@ Zzdatatype.Datatype.List.split_by "\n" Expr.to_string *)
  (*   @@ Goal.get_formulas g *)
  (* in *)
  let _ = Solver.add solver (get_formulas g) in

  (* Solver.to_string solver |> print_endline; *)
  (*  let _, res = Sugar.clock (fun () -> solver_result solver) in *)
  let res = run_z3_in_process solver in
  res

let extend =
  [
    ("len", [ "hd"; "tl"; "emp" ]);
    ("leaf", [ "root"; "lch"; "rch" ]);
    ( "rb_root",
      [
        "rb_leaf";
        "rb_root_color";
        "num_black";
        "no_red_red";
        "rb_lch";
        "rb_rch";
      ] );
    ( "rb_leaf",
      [
        "rb_root";
        "rb_root_color";
        "num_black";
        "no_red_red";
        "rb_lch";
        "rb_rch";
      ] );
    ( "num_black",
      [
        "rb_root"; "rb_root_color"; "rb_leaf"; "no_red_red"; "rb_lch"; "rb_rch";
      ] );
    ( "no_red_red",
      [ "rb_root"; "rb_root_color"; "rb_leaf"; "num_black"; "rb_lch"; "rb_rch" ]
    );
    ( "typing",
      [
        "is_const";
        "is_var";
        "is_abs";
        "is_app";
        "num_app";
        "stlc_ty_nat";
        "stlc_ty_arr1";
        "stlc_ty_arr2";
        "stlc_const";
        "stlc_id";
        "stlc_app1";
        "stlc_app2";
        "stlc_abs_ty";
        "stlc_abs_body";
        "stlc_tyctx_emp";
        "stlc_tyctx_hd";
        "stlc_tyctx_tl";
      ] );
  ]

let query_counter = ref 0

let smt_neg_and_solve ctx axioms vc =
  query_counter := !query_counter + 1;
  let open Language.FrontendTyped in
  let current_mps = prop_get_mp vc in
  let current_mps =
    List.concat
    @@ List.map
         (fun mp ->
           match
             List.find_opt (fun (name, _) -> String.equal name mp) extend
           with
           | Some (_, res) -> mp :: res
           | _ -> [ mp ])
         current_mps
  in
  (* let _ = *)
  (*   Printf.printf "current_mps: %s\n" *)
  (*     (Zzdatatype.Datatype.StrList.to_string current_mps) *)
  (* in *)
  let axioms =
    List.filter
      (fun a ->
        let mps = prop_get_mp a in
        List.for_all (fun mp -> List.exists (String.equal mp) current_mps) mps)
      axioms
  in

  (*   let () = Printf.printf "Num of axioms: %i\n" (List.length axioms) in *)

  (* let () = List.iter (fun a -> Printf.printf "%s\n" (layout_prop a)) axioms in *)

  (* let () = failwith "end" in *)
  let assertions = List.map (Propencoding.to_z3 ctx) (axioms @ [ Not vc ]) in

  (* let () =
       List.iter (fun a -> Printf.printf "%s\n" (Expr.to_string a)) assertions
     in *)
  (*   print_endline "End Axioms"; *)
  let time_t, res = Sugar.clock (fun () -> smt_solve ctx assertions) in
  let () =
    Env.show_debug_stat @@ fun _ -> Pp.printf "Z3 solving time: %0.4fs\n" time_t
  in
  res

exception SMTTIMEOUT

let debug_counter = ref 0
let smt_timeout_flag = ref false

(** Unsat means true; otherwise means false *)
let handle_check_res query_action =
  let time_t, res = Sugar.clock query_action in
  let () =
    Env.show_debug_stat @@ fun _ ->
    Pp.printf "@{<bold>Solving time: %.2f@}\n" time_t
  in
  (* let () = *)
  (*   if 18 == !debug_counter then failwith "end" *)
  (*   else debug_counter := !debug_counter + 1 *)
  (* in *)
  smt_timeout_flag := false;
  match res with
  | SmtUnsat -> true
  | SmtSat model ->
      ( Env.show_log "model" @@ fun _ ->
        Printf.printf "model:\n%s\n"
        @@ Sugar.short_str 1000 @@ Z3.Model.to_string model );
      false
  | Timeout ->
      (Env.show_debug_queries @@ fun _ -> Pp.printf "@{<bold>SMTTIMEOUT@}\n");
      smt_timeout_flag := true;
      false
