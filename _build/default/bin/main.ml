module Env = Zzenv
(* open Language.FrontendTyped *)


let () = print_endline "Hello, World!"

(* $ ./main.exe print-coverage-types meta-config.json data/benchmark/quickchick/sizedlist/_under.ml *)


let process meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = Commands.Cre.preprocess source_file () in
  code

type bird = 
| Chicken of {name: string; call: string}
| Duck of {name:string}

let example_bird = Chicken {name = "Henrietta"; call = "Cluck cluck"}
let _ = Duck {name = "duck"}

let bird_name = match example_bird with
| Chicken {name; _} -> name
| Duck {name} -> name

let () = print_endline bird_name


let _ =
  (* let argc = Array.length Sys.argv in *)

  let config = Array.get Sys.argv 1 in
  let source = Array.get Sys.argv 2 in

  (* process config source () *)

  let code = process config source () in

  let first = (Array.get (Array.of_list code) 0) in

  let name =
    match first with
    | Language.MTyDecl {type_name;_} -> "type dec " ^ type_name
    | Language.MValDecl {x = name; _} -> "val dec" ^ name
    | Language.MMethodPred {x = name; _} -> "method pred " ^ name
    | Language.MAxiom {name;_} -> "axiom " ^ name
    | Language.MFuncImpRaw {name = {x = name; _};_} -> "Function Imp Raw" ^ name
    | Language.MFuncImp {name = {x = name; _};_} -> "Function Imp "^ name
    | Language.MRty {name;_} -> "Rty " ^ name
  in
  (* let term = if is_mtydecl then "True" else "False" *)

  let get_body = function
    | Language.MFuncImp {body; _} -> body
    | _ -> failwith "Unsupported variant for extracting body"
  in

  let body = get_body first in

  let body_str = Language.FrontendTyped.layout_typed_term body in

  print_endline name;
  let () = print_endline body_str
  in ()



  (* print_int (List.length code); *)
  (* assert (List.length first == 1); *)
  (* let item = (Array.get (Array.of_list code) 0) in *)
  (* let _, code_rty = Language.get_rty_by_name first "test" in *)
  (* Language.get_rty_by_name code "test" *)
  (* print_endline (Language.FrontendTyped.layout_rty code_rty); *)

  (* List.iter Language.FrontendTyped.layout_typed_term code *)
  (* Language.FrontendTyped.layout_rty (Array.get (Array.of_list code) 0) *)

  

(* let string = Cobb.Postprocess.final_program_to_string  *)


(* ../Cobb/underapproximation_type/meta-config.json  *)
(* ../Cobb/underapproximation_type/data/benchmark/quickchick/sizedlist/_under.ml *)