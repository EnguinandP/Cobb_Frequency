open Mtyped
open Term
module Env = Zzenv

(** calls poirot to get AST of source_file *)
let process meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = Commands.Cre.preprocess source_file () in
  code

(** returns "frequency_gen" as ('t, string) typed *)
let replace_bool_gen_string (s : ('t, string) typed) : ('t, string) typed = 
  s #-> (function _ -> "frequency_gen")

let is_bool_gen (s : ('t, string) typed) : bool =
  s.x = "bool_gen"

(** recursively traverses through AST to find and replace bool_gen with frequency_gen *)
let rec replace_bool_gen (t : ('t, 't term) typed) : ('t, 't term) typed = 
  t #-> ( function
  | CErr -> CErr
  | CVal t -> CVal t #-> replace_bool_gen_value
  | CLetE { lhs; rhs; body} ->
    CLetE {
      lhs;
      rhs = replace_bool_gen rhs;
      body = replace_bool_gen body
    }
  | CLetDeTu { turhs; tulhs; body} ->
    CLetDeTu {
      turhs = turhs #-> replace_bool_gen_value;
      tulhs; 
      body = replace_bool_gen body;
    }
  | CApp { appf; apparg} ->
    CApp {
      appf = appf #-> replace_bool_gen_value;
      apparg = apparg #-> replace_bool_gen_value;
    }
  | CAppOp {op; appopargs} -> 
    CAppOp {
      op;
      appopargs = (List.map (function y -> y #-> replace_bool_gen_value ) appopargs)
    }
  | CMatch
  (* To rewrite matches on True/False to true/false, from Cobb_postprocess*)
  {
    matched;
    match_cases =
      [
        CMatchcase
          { constructor = { x = "True"; ty = ty_t }; args = []; exp = exp1 };
        CMatchcase
          {
            constructor = { x = "False"; ty = ty_f };
            args = [];
            exp = exp2;
          };
      ];
  } -> 
    CMatch
        {
          matched;
          match_cases =
            [
              CMatchcase
                {
                  constructor = "true"#:ty_t;
                  args = [];
                  exp = replace_bool_gen exp1;
                };
              CMatchcase
                {
                  constructor = "false"#:ty_f;
                  args = [];
                  exp = replace_bool_gen exp2;
                };
            ];
        }
  | CMatch { matched; match_cases } ->
    CMatch {
      matched = matched #-> replace_bool_gen_value;
      match_cases =
        List.map (function (CMatchcase {constructor; args; exp}) -> 
          CMatchcase {
            constructor;
            args;
            exp = replace_bool_gen exp;
          }
        )
        match_cases
    }
  )
and replace_bool_gen_value (v : 't value) =
  match v with 
  | VConst _ -> v
  (* bool_gen is a VVar *)
  | VVar s -> 
    if is_bool_gen s then
      VVar (replace_bool_gen_string s)
    else
      VVar s
  | VLam {lamarg; body} -> 
    VLam  {
      lamarg;
      body = replace_bool_gen body;
    }
  | VFix { fixname; fixarg; body } -> 
    VFix {
      fixname;
      fixarg;
      body = replace_bool_gen body;
    }
  | VTu l -> 
    VTu (List.map (function y -> y #-> replace_bool_gen_value ) l)

(* #-> applies function to arg, returning it as `typed` *)

(** gets the body of the function *)
let get_name_rec_body = function
| Language.MFuncImp {name; if_rec;body; _} -> (name, if_rec, body) 
| _ -> failwith "Unsupported variant for extracting body"

(* some initial code for understanding the AST *)
let example_term_1 : 't term = CErr
let example_term_2 : (('t, 't term) typed) = { x = CVal {x = VConst (I 42); ty = 0}; ty = 0}
let example_term_3 : (('t, 't term) typed) = { x = CErr; ty = 0}

let get_value_constructor (v : 't value) =
  match v with 
  | VConst _-> "const"
  | VVar _ -> "var"
  | VLam _ -> "lam"
  | VFix _ -> "fix"
  | VTu _ -> "tu"

(* finds what type of term bool_gen is (CVal) *)
(* matches for term *)
let peek (t : ('t, 't term) typed) =
  match t.x with 
  | CVal {x = value;_} -> get_value_constructor value
  | _ -> failwith "not CVal"


(* some functions to help with printing *)
let print_string_list l = List.iter print_endline l
let print_string_typed (s : ('t, string) typed ) = print_endline s.x
let print_string_typed_list (l : ('t, string) typed list) = List.iter print_string_typed l

let print_code (config:string) (source:string) =

  (* calls poirot preprocess to get AST of source file *)
  let code = process config source () in

  (* gets the first item, (gets the function and ignores the type annotation) *)
  let first_item = (Array.get (Array.of_list code) 0) in

  (* finds which type of item (MFuncImp) *)
  (* let name =
    match first_item with
    | Language.MTyDecl {type_name;_} -> "type dec " ^ type_name
    | Language.MValDecl {x = name; _} -> "val dec" ^ name
    | Language.MMethodPred {x = name; _} -> "method pred " ^ name
    | Language.MAxiom {name;_} -> "axiom " ^ name
    | Language.MFuncImpRaw {name = {x = name; _};_} -> "Function Imp Raw" ^ name
    | Language.MFuncImp {name = {x = name; _};_} -> "Function Imp "^ name
    | Language.MRty {name;_} -> "Rty " ^ name
  in *)
  (* let term = if is_mtydecl then "True" else "False" *)

  (* converts the body into terms *)
  let (name, if_rec, body) = get_name_rec_body first_item in

  (* gets string of AST *)
  let body_str = Language.FrontendTyped.layout_typed_term body in

  
  print_endline name.x;
  print_endline body_str;
  let () = print_endline (peek body) 
in ()

(** returns string version of AST *)
let final_program_to_string name if_rec new_body : string = 
  let body_as_item = 
    Item.MFuncImp
      {
        name = name;
        if_rec = if_rec;
        body = new_body;
      }
  in
  let reconstructed_body = Item.map_item (fun x -> Some x) body_as_item in
  Frontend_opt.To_item.layout_item reconstructed_body


let transform_program (config : string) (source : string ) = 
  
  let code = process config source () in

  (* gets the first item, (gets the function and ignores the type annotation) *)
  let first_item = (Array.get (Array.of_list code) 0) in

  (* converts the body into terms *)
  let (name, if_rec, body) = get_name_rec_body first_item in

  (* replaces bool_gen with frequency_gen*)
  let new_body = replace_bool_gen body in
  let new_code = final_program_to_string name if_rec new_body in
  
  (* prints new body *)
  (* let () = print_endline (Language.FrontendTyped.layout_typed_term new_body) in *)

  let () = print_endline new_code in

  (* prints program to file *)
  let filename = String.sub source 0 ( (String.length source) - 3) ^ "_trans.ml" in
  let oc = open_out filename in
  output_string oc "open Combinators\n";
  output_string oc "open Frequency_combinators\n";
  output_string oc new_code;
  close_out oc

let () =
  try 
    let config_file = "meta-config.json" in
    let source_file = Array.get Sys.argv 1 in

    transform_program config_file source_file 
  with
  | Invalid_argument s -> print_endline "Usage: dune exec transformation program_file"