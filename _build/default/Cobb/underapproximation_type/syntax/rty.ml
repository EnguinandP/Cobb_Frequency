open Sexplib.Std
open Mtyped
open Cty

type 't rty =
  | RtyBase of { ou : bool; cty : 't cty }
  | RtyBaseArr of { argcty : 't cty; arg : (string[@bound]); retty : 't rty }
  | RtyArrArr of { argrty : 't rty; retty : 't rty }
  | RtyTuple of 't rty list
[@@deriving sexp]

(* NOTE: modified *)
let rec fv_rty (rty_e : 't rty) =
  match rty_e with
  | RtyBase { cty; _ } -> [] @ fv_cty cty
  | RtyBaseArr { argcty; arg; retty } ->
      let res = [] @ fv_rty retty in
      let res =
        List.filter_map
          (fun x -> if String.equal arg x.x then None else Some x)
          res
      in
      res @ fv_cty argcty
  | RtyArrArr { argrty; retty } -> ([] @ fv_rty retty) @ fv_rty argrty
  | RtyTuple _trtylist0 -> [] @ List.concat (List.map fv_rty _trtylist0)

and typed_fv_rty (rty_e : ('t, 't rty) typed) = fv_rty rty_e.x

let rec subst_rty (string_x : string) f (rty_e : 't rty) =
  match rty_e with
  | RtyBase { ou; cty } -> RtyBase { ou; cty = subst_cty string_x f cty }
  | RtyBaseArr { argcty; arg; retty } ->
      if String.equal arg string_x then
        RtyBaseArr { argcty = subst_cty string_x f argcty; arg; retty }
      else
        RtyBaseArr
          {
            argcty = subst_cty string_x f argcty;
            arg;
            retty = subst_rty string_x f retty;
          }
  | RtyArrArr { argrty; retty } ->
      RtyArrArr
        {
          argrty = subst_rty string_x f argrty;
          retty = subst_rty string_x f retty;
        }
  | RtyTuple _trtylist0 -> RtyTuple (List.map (subst_rty string_x f) _trtylist0)

and typed_subst_rty (string_x : string) f (rty_e : ('t, 't rty) typed) =
  rty_e #-> (subst_rty string_x f)

let rec map_rty (f : 't -> 's) (rty_e : 't rty) =
  match rty_e with
  | RtyBase { ou; cty } -> RtyBase { ou; cty = map_cty f cty }
  | RtyBaseArr { argcty; arg; retty } ->
      RtyBaseArr { argcty = map_cty f argcty; arg; retty = map_rty f retty }
  | RtyArrArr { argrty; retty } ->
      RtyArrArr { argrty = map_rty f argrty; retty = map_rty f retty }
  | RtyTuple _trtylist0 -> RtyTuple (List.map (map_rty f) _trtylist0)

and typed_map_rty (f : 't -> 's) (rty_e : ('t, 't rty) typed) =
  rty_e #=> f #-> (map_rty f)

let fv_rty_id e = fv_typed_id_to_id fv_rty e
let typed_fv_rty_id e = fv_typed_id_to_id typed_fv_rty e
let subst_rty_instance x instance e = subst_f_to_instance subst_rty x instance e

let typed_subst_rty_instance x instance e =
  subst_f_to_instance typed_subst_rty x instance e
(* Generated from _rty.ml *)

let rec erase_rty = function
  | RtyBase { cty; _ } -> erase_cty cty
  | RtyBaseArr { argcty; arg; retty } ->
      Nt.mk_arr (erase_cty argcty) (erase_rty retty)
  | RtyArrArr { argrty; retty } ->
      Nt.mk_arr (erase_rty argrty) (erase_rty retty)
  | RtyTuple _trtylist0 -> Nt.mk_tuple (List.map erase_rty _trtylist0)

let is_base_rty = function RtyBase _ -> true | _ -> false

let assume_base_rty = function
  | RtyBase { ou; cty } -> (ou, cty)
  | _ -> failwith "assume_base_rty"

let ou_to_qt = function
  | true -> Normalty.Connective.Fa
  | false -> Normalty.Connective.Ex

let qt_to_ou = function
  | Normalty.Connective.Fa -> true
  | Normalty.Connective.Ex -> false
