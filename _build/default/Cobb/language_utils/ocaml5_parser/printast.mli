# 1 "Cobb/language_utils/ocaml5_parser/parsing/printast.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*              Damien Doligez, projet Para, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Raw printer for {!Parsetree}

  {b Warning:} this module is unstable and part of
  {{!Compiler_libs}compiler-libs}.

*)

open Parsetree
open Format

val interface : formatter -> signature_item list -> unit
val implementation : formatter -> structure_item list -> unit
val top_phrase : formatter -> toplevel_phrase -> unit

val expression: int -> formatter -> expression -> unit
val structure: int -> formatter -> structure -> unit
val payload: int -> formatter -> payload -> unit
