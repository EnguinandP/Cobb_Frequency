open Mtyped
open Term
module Env = Zzenv
(* open Language.FrontendTyped *)

let process meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = Commands.Cre.preprocess source_file () in
  code

let example_term_1 : 't term = CErr
let example_term_2 : (('t, 't term) typed) = { x = CVal {x = VConst (I 42); ty = 0}; ty = 0}

let get_value_constructor (v : 't value) =
  match v with 
  | VConst _-> "const"
  | VVar _ -> "var"
  | VLam _ -> "lam"
  | VFix _ -> "fix"
  | VTu _ -> "tu"

(* TODO: should recursively search in term until bool_gen *)
let find_bool_gen (v : 't value) =
  match v with 
  | VFix { fixname; fixarg; body } -> 
    VFix {
      fixname;
      fixarg;
      body = example_term_2;
    }
  | _ -> v

(* finds what type of term bool_gen is (CVal) *)
let peek (t : ('t, 't term) typed) =
  match t.x with 
  (* | CVal v -> typed_fv_value_id v *)
  (* | CVal {x = value;_} -> fv_value value *)
  | CVal {x = value;_} -> get_value_constructor value
  | _ -> failwith "not CVal"


(* some functions to help with printing *)
let print_string_list l = List.iter print_endline l
let print_string_typed (s : ('t, string) typed ) = print_endline s.x
let print_string_typed_list (l : ('t, string) typed list) = List.iter print_string_typed l
  
let print_code (config:string) (source:string) =

  (* let config = Array.get Sys.argv 1 in
  let source = Array.get Sys.argv 2 in *)

  (* calls poirot preprocess to get AST of source file *)
  let code = process config source () in

  (* gets the first item, (gets the function and ignores the type annotation) *)
  let first_item = (Array.get (Array.of_list code) 0) in

  (* finds which type of item (MFuncImp) *)
  let name =
    match first_item with
    | Language.MTyDecl {type_name;_} -> "type dec " ^ type_name
    | Language.MValDecl {x = name; _} -> "val dec" ^ name
    | Language.MMethodPred {x = name; _} -> "method pred " ^ name
    | Language.MAxiom {name;_} -> "axiom " ^ name
    | Language.MFuncImpRaw {name = {x = name; _};_} -> "Function Imp Raw" ^ name
    | Language.MFuncImp {name = {x = name; _};_} -> "Function Imp "^ name
    | Language.MRty {name;_} -> "Rty " ^ name
  in
  (* let term = if is_mtydecl then "True" else "False" *)

  (* gets the body of the function *)
  let get_body = function
    | Language.MFuncImp {body; _} -> body
    | _ -> failwith "Unsupported variant for extracting body"
  in

  let body = get_body first_item in

  (* prints out body *)
  let body_str = Language.FrontendTyped.layout_typed_term body in

  print_endline name;
  print_endline body_str;
  let () = print_endline (peek body)
  (* let () = print_string_typed_list (peek body) *)
  (* let () = print_string_list (peek body) *)
  (* print_string_list (find_bool_gen body) *)
  in ()

let () = 
  print_code "meta-config.json" "bin/source_file_ex.ml";



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