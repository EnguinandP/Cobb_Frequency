open Combinators

let test_count = 20000
let test_max_fail = 20000

let precondition_frequency prop (gen_type, name) =
  QCheck.(
    Test.make ~count:test_count ~max_fail:test_max_fail ~name
      (QCheck.make (fun _ -> gen_type ()))
      (fun l ->
        assume (prop l);
        true))

let sized_list_generators =
  [
    (Generators.Sizedlist_trans.sized_list_gen, "prog_trans");
    (* (Examples.Sizedlist.sized_list_gen, "prog"); *)
  ]

let sized_list_arbitraries =
  List.map
    (fun (gen, name) ->
      ( (fun () ->
          let size = nat_gen () in
          (size, gen size)),
        name ))
    sized_list_generators

let eval_sized_list =
  ( 
    (* "sized_list", *)
    List.map
      (precondition_frequency Precondition.is_sized)
      sized_list_arbitraries )

let run_qcheck foldername = 
  let filename = foldername ^ "results" ^ ".result" in
  print_endline ("> Running test for " ^ "t_get_name" ^ "...");
  let oc = open_out filename in
  ignore (QCheck_runner.run_tests ~verbose:true ~out:oc eval_sized_list);
  close_out oc