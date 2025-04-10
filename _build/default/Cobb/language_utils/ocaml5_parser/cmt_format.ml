# 1 "Cobb/language_utils/ocaml5_parser/file_formats/cmt_format.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmi_format
open Typedtree

(* Note that in Typerex, there is an awful hack to save a cmt file
   together with the interface file that was generated by ocaml (this
   is because the installed version of ocaml might differ from the one
   integrated in Typerex).
*)



let read_magic_number ic =
  let len_magic_number = String.length Config.cmt_magic_number in
  really_input_string ic len_magic_number

type binary_annots =
  | Packed of Types.signature * string list
  | Implementation of structure
  | Interface of signature
  | Partial_implementation of binary_part array
  | Partial_interface of binary_part array

and binary_part =
| Partial_structure of structure
| Partial_structure_item of structure_item
| Partial_expression of expression
| Partial_pattern : 'k pattern_category * 'k general_pattern -> binary_part
| Partial_class_expr of class_expr
| Partial_signature of signature
| Partial_signature_item of signature_item
| Partial_module_type of module_type

type cmt_infos = {
  cmt_modname : string;
  cmt_annots : binary_annots;
  cmt_value_dependencies :
    (Types.value_description * Types.value_description) list;
  cmt_comments : (string * Location.t) list;
  cmt_args : string array;
  cmt_sourcefile : string option;
  cmt_builddir : string;
  cmt_loadpath : string list;
  cmt_source_digest : Digest.t option;
  cmt_initial_env : Env.t;
  cmt_imports : (string * Digest.t option) list;
  cmt_interface_digest : Digest.t option;
  cmt_use_summaries : bool;
  cmt_uid_to_loc : Location.t Shape.Uid.Tbl.t;
  cmt_impl_shape : Shape.t option; (* None for mli *)
}

type error =
    Not_a_typedtree of string

let need_to_clear_env =
  try ignore (Sys.getenv "OCAML_BINANNOT_WITHENV"); false
  with Not_found -> true

let keep_only_summary = Env.keep_only_summary

open Tast_mapper

let cenv =
  {Tast_mapper.default with env = fun _sub env -> keep_only_summary env}

let clear_part = function
  | Partial_structure s -> Partial_structure (cenv.structure cenv s)
  | Partial_structure_item s ->
      Partial_structure_item (cenv.structure_item cenv s)
  | Partial_expression e -> Partial_expression (cenv.expr cenv e)
  | Partial_pattern (category, p) -> Partial_pattern (category, cenv.pat cenv p)
  | Partial_class_expr ce -> Partial_class_expr (cenv.class_expr cenv ce)
  | Partial_signature s -> Partial_signature (cenv.signature cenv s)
  | Partial_signature_item s ->
      Partial_signature_item (cenv.signature_item cenv s)
  | Partial_module_type s -> Partial_module_type (cenv.module_type cenv s)

let clear_env binary_annots =
  if need_to_clear_env then
    match binary_annots with
    | Implementation s -> Implementation (cenv.structure cenv s)
    | Interface s -> Interface (cenv.signature cenv s)
    | Packed _ -> binary_annots
    | Partial_implementation array ->
        Partial_implementation (Array.map clear_part array)
    | Partial_interface array ->
        Partial_interface (Array.map clear_part array)

  else binary_annots

exception Error of error

let input_cmt ic = (input_value ic : cmt_infos)

let output_cmt oc cmt =
  output_string oc Config.cmt_magic_number;
  output_value oc (cmt : cmt_infos)

let read filename =
(*  Printf.fprintf stderr "Cmt_format.read %s\n%!" filename; *)
  let ic = open_in_bin filename in
  Misc.try_finally
    ~always:(fun () -> close_in ic)
    (fun () ->
       let magic_number = read_magic_number ic in
       let cmi, cmt =
         if magic_number = Config.cmt_magic_number then
           None, Some (input_cmt ic)
         else if magic_number = Config.cmi_magic_number then
           let cmi = Cmi_format.input_cmi ic in
           let cmt = try
               let magic_number = read_magic_number ic in
               if magic_number = Config.cmt_magic_number then
                 let cmt = input_cmt ic in
                 Some cmt
               else None
             with _ -> None
           in
           Some cmi, cmt
         else
           raise(Cmi_format.Error(Cmi_format.Not_an_interface filename))
       in
       cmi, cmt
    )

let read_cmt filename =
  match read filename with
      _, None -> raise (Error (Not_a_typedtree filename))
    | _, Some cmt -> cmt

let read_cmi filename =
  match read filename with
      None, _ ->
        raise (Cmi_format.Error (Cmi_format.Not_an_interface filename))
    | Some cmi, _ -> cmi

let saved_types = ref []
let value_deps = ref []

let clear () =
  saved_types := [];
  value_deps := []

let add_saved_type b = saved_types := b :: !saved_types
let get_saved_types () = !saved_types
let set_saved_types l = saved_types := l

let record_value_dependency vd1 vd2 =
  if vd1.Types.val_loc <> vd2.Types.val_loc then
    value_deps := (vd1, vd2) :: !value_deps

let save_cmt filename modname binary_annots sourcefile initial_env cmi shape =
  if !Clflags.binary_annotations && not !Clflags.print_types then begin
    Misc.output_to_file_via_temporary
       ~mode:[Open_binary] filename
       (fun temp_file_name oc ->
         let this_crc =
           match cmi with
           | None -> None
           | Some cmi -> Some (output_cmi temp_file_name oc cmi)
         in
         let source_digest = Option.map Digest.file sourcefile in
         let cmt = {
           cmt_modname = modname;
           cmt_annots = clear_env binary_annots;
           cmt_value_dependencies = !value_deps;
           cmt_comments = Lexer.comments ();
           cmt_args = Sys.argv;
           cmt_sourcefile = sourcefile;
           cmt_builddir = Location.rewrite_absolute_path (Sys.getcwd ());
           cmt_loadpath = Load_path.get_paths ();
           cmt_source_digest = source_digest;
           cmt_initial_env = if need_to_clear_env then
               keep_only_summary initial_env else initial_env;
           cmt_imports = List.sort compare (Env.imports ());
           cmt_interface_digest = this_crc;
           cmt_use_summaries = need_to_clear_env;
           cmt_uid_to_loc = Env.get_uid_to_loc_tbl ();
           cmt_impl_shape = shape;
         } in
         output_cmt oc cmt)
  end;
  clear ()
