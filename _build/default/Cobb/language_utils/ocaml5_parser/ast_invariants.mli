# 1 "Cobb/language_utils/ocaml5_parser/parsing/ast_invariants.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2015 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Check AST invariants

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

val structure : Parsetree.structure -> unit
val signature : Parsetree.signature -> unit
