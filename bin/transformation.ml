open Mtyped
open Term
module Env = Zzenv

(* TODO: add different transformation for multiple bool_gens *)

(** calls poirot to get AST of source_file *)
let process meta_config_file source_file () =
  let () = Env.load_meta meta_config_file in
  let code = Commands.Cre.preprocess source_file () in
  code



(** recursively traverses through AST to find if it calls the recusive function specified by name *)
let rec has_recursive_call (t : ('t, 't term) typed) (name : string) =
match t.x with
  | CErr -> false
  | CVal t -> has_recursive_call_value name t.x
  | CLetE { lhs; rhs; body} -> 
      (has_recursive_call rhs name) &&
      (has_recursive_call body name)
  | CLetDeTu { turhs; tulhs; body} ->
      (has_recursive_call_value name turhs.x) &&
      (has_recursive_call body name)
  | CApp { appf; apparg} ->
      (has_recursive_call_value name appf.x) &&
      (has_recursive_call_value name apparg.x);
  | CAppOp {op; appopargs} -> 
      List.fold_left (fun (acc:bool) x -> 
            acc || (has_recursive_call_value name x.x)) false appopargs
  | CMatch { matched; match_cases } ->
    (has_recursive_call_value name matched.x) &&
        List.fold_left (fun (acc:bool) (CMatchcase {constructor; args; exp}) -> 
            acc || (has_recursive_call exp name)) false match_cases
and has_recursive_call_value (name : string) (v : 't value) =
  match v with 
  | VConst _ -> has_recursive_call_value name v
  (* bool_gen is a VVar *)
  | VVar s ->
    if s.x = name then
      true
    else
      false
  | VLam {lamarg; body} -> 
      has_recursive_call body name
  | VFix { fixname; fixarg; body } -> 
      has_recursive_call body name
  | VTu l -> 
    List.fold_left (fun (acc:bool) x -> 
      acc || (has_recursive_call_value name x.x)) false l

(** returns "frequency_gen" as ('t, string) typed *)
let replace_bool_gen_string (s : ('t, string) typed) : ('t, string) typed = 
  s #-> (function _ -> "frequency_gen_list")

let is_bool_gen (s : ('t, string) typed) : bool =
  s.x = "bool_gen"

(** recursively traverses through AST to find and replace bool_gen with frequency_gen *)
let rec replace_bool_gen (t : ('t, 't term) typed) (name : string) : ('t, 't term) typed = 
  t #-> ( function
  | CErr ->     (* raise Bailout *)
    CVal { x = VVar ("raise BailOut" #: Nt.Ty_any); ty = Nt.Ty_any}
  | CVal t -> CVal t #-> (replace_bool_gen_value name)
  (* thunkifies branches *)
  | CLetE { 
      lhs; 
      rhs = { x = CApp { appf = { x = VVar {x = "bool_gen"; ty }; ty = ty2} ; apparg }; ty = ty3 }; 
      body = { x = CMatch { matched ; match_cases = [
        CMatchcase
          { 
            constructor = { x = "True"; ty = ty_t }; 
            args = []; 
            exp = exp1;
          };
        CMatchcase
          {
            constructor = { x = "False"; ty = ty_f };
            args = [];
            exp = exp2;
          };
      ]; }; ty = ty4} } ->
        CLetE {     (* w_base = get_weight_idx 0 *)
          lhs = ("w_base" #: ty);
          rhs = { x = CApp {
            appf = { x = VVar ("get_weight_idx" #: ty); ty = Nt.Ty_any};
            apparg = {x = VConst (I 0); ty = Nt.Ty_any};
          }; ty = Nt.Ty_any}; 
          body = { x = CLetE {    (* w_recursive = get_weight_idx 1 *)
            lhs = ("w_recursive" #: ty);
            rhs = { x = CApp {
              appf = { x = VVar ("get_weight_idx" #: ty); ty = Nt.Ty_any};
              apparg = {x = VConst (I 1); ty = Nt.Ty_any};
            }; ty = Nt.Ty_any}; 
            body = { x = CLetE {    (* let (base_case) = frequency_gen (w_base, []) *)
              lhs = ("base_case" #: ty);
              rhs = { x = CApp { 
                appf = { x = VVar (replace_bool_gen_string "bool_gen"#:ty); ty = ty2}; 
                apparg = { x = VTu [
                  { x = VVar ("w_base" #: Nt.Ty_any); ty = Nt.Ty_any};
                  { x = VLam {
                      lamarg = ("_" #: ty); 
                      body = 
                        if not (has_recursive_call exp1 name) then
                          exp1 
                        else 
                          exp2;
                  }; ty = Nt.Ty_any}; ]    
                  ; ty = Nt.Ty_any (* placeholder *) }
                }; ty = ty3 }; 
              body = { x = CLetE {    (* let (recursive_case) = base_case (w_base, ...) *)
                lhs = ("recursive_case" #: ty);
                rhs = { x = CApp {
                  appf = { x = VVar ("base_case" #: ty); ty = Nt.Ty_any};
                  apparg = { x = VTu [
                    { x = VVar ("w_recursive" #: Nt.Ty_any); ty = Nt.Ty_any};
                    { x = VLam {
                        lamarg = ("_" #: ty); 
                        body = 
                          if (has_recursive_call exp1 name) then
                            exp1 
                          else 
                            exp2;
                    }; ty = Nt.Ty_any}; ]    
                    ; ty = Nt.Ty_any (* placeholder *) }
                }; ty = Nt.Ty_any};
                body = { x = CVal { x = VVar ("recursive_case" #: ty); ty = Nt.Ty_any} ; ty = Nt.Ty_any}
              }; ty = Nt.Ty_any (* placeholder *) };
            }; ty = Nt.Ty_any}
          }; ty = Nt.Ty_any}}
  | CLetE { lhs; rhs; body} -> 
    CLetE {
      lhs;
      rhs = replace_bool_gen rhs name;
      body = replace_bool_gen body name
    }
  | CLetDeTu { turhs; tulhs; body} ->
    CLetDeTu {
      turhs = turhs #-> (replace_bool_gen_value name);
      tulhs; 
      body = replace_bool_gen body name;
    }
  | CApp { appf; apparg} ->
    CApp {
      appf = appf #-> (replace_bool_gen_value name);
      apparg = apparg #-> (replace_bool_gen_value name);
  }
  | CAppOp {op; appopargs} -> 
    CAppOp {
      op;
      appopargs = (List.map (function y -> y #-> (replace_bool_gen_value name) ) appopargs)
    }
  | CMatch
  (* To rewrite matches on True/False to true/false, from Cobb_postprocess*)
  {
    matched;
    match_cases =
      [
        CMatchcase
          { 
            constructor = { x = "True"; ty = ty_t }; 
            args = []; 
            exp = exp1 
          };
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
                  exp = replace_bool_gen exp1 name;
                };
              CMatchcase
                {
                  constructor = "false"#:ty_f;
                  args = [];
                  exp = replace_bool_gen exp2 name;
                };
            ];
        }
  | CMatch { matched; match_cases } ->
    CMatch {
      matched = matched #-> (replace_bool_gen_value name);
      match_cases =
        List.map (function (CMatchcase {constructor; args; exp}) -> 
          CMatchcase {
            constructor;
            args;
            exp = replace_bool_gen exp name;
          }
        )
        match_cases
    }
  )
and replace_bool_gen_value (name : string) (v : 't value) =
  match v with 
  | VConst _ -> v
  (* bool_gen is a VVar *)
  | VVar s -> (* TODO: rewrite this *)
    if s.x = "bool_gen" then
      VVar (replace_bool_gen_string s)
    else
      VVar s
  | VLam {lamarg; body} -> 
    VLam  {
      lamarg;
      body = replace_bool_gen body name;
    }
  | VFix { fixname; fixarg; body } -> 
    VFix {
      fixname;
      fixarg;
      body = replace_bool_gen body name;
    }
  | VTu l -> 
    (* tuples *)
    VTu (List.map (function y -> y #-> (replace_bool_gen_value name)) l)

(* #-> applies function to arg, returning it as `typed` *)

(** gets the body of the function *)
let get_function = function
| Language.MFuncImp {name; if_rec;body; _} -> Some (name, if_rec, body) 
| _ -> None

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

let print_code (config:string) (source:string) =

  (* calls poirot preprocess to get AST of source file *)
  let code = process config source () in

  (* gets the first item, (gets the function and ignores the type annotation) *)
  let first_item = (Array.get (Array.of_list code) 0) in

  (* converts the body into terms *)
  let (name, if_rec, body) = 
    match get_function first_item with
    | Some (name, if_rec, body) -> (name, if_rec, body)
    | None -> failwith "Expected a function but got None"
  in

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
  (* Change to (fun x -> None) to remove type annotations and improve clarity *)
  let reconstructed_body = Item.map_item (fun x -> None) body_as_item in
  Frontend_opt.To_item.layout_item reconstructed_body


let frequify_program (config : string) (source : string ) = 
  (* try  *)

  (* let ic = open_in source in  *)

  let code = process config source () in
  let code_arr = Array.of_list code in

  (* (gets the function and ignores the type annotation) *)
  (* print_int (Array.length code_arr); *)
  for x = 0 to (Array.length code_arr) - 1 do
    match get_function code_arr.(x) with
    | Some (name, if_rec, body) -> 
      (* replaces bool_gen with frequency_gen*)
      let new_body = replace_bool_gen body name.x in
      let new_code = final_program_to_string name if_rec new_body in
      
      (* prints new body *)
      (* let () = print_endline new_code in *)

      (* prints program to file *)
      let filename = String.sub source 0 ( (String.length source) - 3) ^ "_freq.ml" in
      let oc = open_out filename in
      output_string oc "open Combinators\n";
      output_string oc "open Frequency_combinators\n";
      output_string oc new_code;
      close_out oc
    | None -> ()
  done
(* with
      | Failure s -> 
        (* processs can't read open ... *)
        print_endline "fixing file";
        let ic = open_in source in
        (* let oc = open_out "temp" in *)

        let lines = ref [] in

        (* throw out first line (open ...) *)
        let _ = input_line ic in

        try
          while true do
            let line = input_line ic in
            (* Printf.fprintf oc "%s\n" line; *)
            print_endline line;
            lines := line :: !lines
          done
        with End_of_file -> close_in ic;

        let oc = open_out source in
        List.iter (fun x -> Printf.fprintf oc "%s\n" x) !lines;
        close_out oc;

        frequify_program config source *)

(* let rewrite_freq (source : string) (w_base : int) (w_recursive : int)=
  let lines = ref [] in
  let ic = open_in source in

  try while true do
    let line = input_line ic in
    (* print_endline line; *)
    if line = "      let (w0) = get_weight_idx 0 in" then
      let l = String.split_on_char '=' line in
      print_endline (Array.of_list l).(0);
      print_endline (Array.of_list l).(1);
      lines := ((Array.of_list l).(0) ^ "= " ^ (string_of_int w_base)) :: !lines
    else
      lines := line :: !lines
  done
  with End_of_file -> close_in ic;

  let oc = open_out source in

  let _ = List.iter (fun x -> Printf.fprintf oc "%s" x) !lines in
  () *)

let source = ref "" 
let all = ref false
let usage_msg = "Usage: dune exec transformation [-a] <program_file>"

let anon_fun file = source := file
let speclist = [
  ("-a", Arg.Set all, "Transform all files in directory")
] 

let () =
  try 
    let config_file = "meta-config.json" in
    
    Arg.parse speclist anon_fun usage_msg;

    if (!all) then
      let files = Sys.readdir !source in
        Array.iter (fun (s:string) ->
          if (String.ends_with ~suffix:".ml" s) && (not (String.ends_with ~suffix:"_freq.ml" s )) then 
            let file = (!source ^ "/" ^ s) in
            Printf.printf "Syntactically transformed %s\n" file;
            frequify_program config_file file
          else ()) 
          files
    else
      frequify_program config_file !source 
  with
  (* | Invalid_argument s -> print_endline "Usage:a dune exec transformation [-a] <program_file>" *)
  | Sys_error s -> print_endline s
  (* | Failure s -> print_endline s *)
