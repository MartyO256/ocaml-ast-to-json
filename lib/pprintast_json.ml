(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Hongbo Zhang (University of Pennsylvania)                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Derived from: https://github.com/ocaml/ocaml/blob/trunk/parsing/pprintast.ml *)

open Asttypes
open Format
open Location
open Longident
open Parsetree

exception Not_implemented

let raise_not_implemented () = raise Not_implemented
let protect_ident ppf txt = fprintf ppf "%s" txt

type space_formatter = (unit, Format.formatter, unit) format

let override = function Override -> "!" | Fresh -> ""

let type_variance = function
  | NoVariance -> ""
  | Covariant -> "+"
  | Contravariant -> "-"

let type_injectivity = function NoInjectivity -> "" | Injective -> "!"
let pp = fprintf

let list :
      'a.
      ?sep:space_formatter ->
      ?first:space_formatter ->
      ?last:space_formatter ->
      (Format.formatter -> 'a -> unit) ->
      Format.formatter ->
      'a list ->
      unit =
 fun ?(sep = ("@ " : _ format6)) ?(first = ("" : _ format6))
     ?(last = ("" : _ format6)) fu f xs ->
  let aux f = function
    | [] -> ()
    | [ x ] -> fu f x
    | xs ->
        let rec loop f = function
          | [ x ] -> fu f x
          | x :: xs ->
              fu f x;
              pp f sep;
              loop f xs
          | _ -> assert false
        in
        pp f first;
        loop f xs;
        pp f last
  in
  aux f xs

let rec longident f = function
  | Lident s ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"identifier\",@;\
         \"variant\": \"long-identifier\",@;\
         \"value\": @,\
         \"%s\"@]@;\
         }@]"
        s
  | Ldot (y, s) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"identifier\",@;\
         \"variant\": \"dot-identifier\",@;\
         \"head\": @,\
         %a,@;\
         \"tail\": @,\
         \"%s\"@]@;\
         }@]"
        longident y s
  | Lapply (y, s) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"identifier\",@;\
         \"variant\": \"apply-identifier\",@;\
         \"head\": @,\
         %a,@;\
         \"tail\": @,\
         %a@]@;\
         }@]"
        longident y longident s

let longident_loc f x = pp f "%a" longident x.txt

let constant f = function
  | Pconst_char i ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"constant\",@;\
         \"variant\": \"character\",@;\
         \"value\": %C@;\
         }@]"
        i
  | Pconst_string (i, _, _) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"constant\",@;\
         \"variant\": \"string\",@;\
         \"value\": %S@;\
         }@]"
        i
  | Pconst_integer (i, None) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"constant\",@;\
         \"variant\": \"integer\",@;\
         \"value\": %s@;\
         }@]"
        i
  | Pconst_integer (i, Some m) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"constant\",@;\
         \"variant\": \"integer\",@;\
         \"value\": %s%c@;\
         }@]"
        i m
  | Pconst_float (i, None) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"constant\",@;\
         \"variant\": \"float\",@;\
         \"value\": %f@;\
         }@]"
        (float_of_string i)
  | Pconst_float (i, Some m) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"constant\",@;\
         \"variant\": \"float\",@;\
         \"value\": %s%c@;\
         }@]"
        i m

let mutable_flag f = function Immutable -> () | Mutable -> pp f "mutable@;"
let virtual_flag f = function Concrete -> () | Virtual -> pp f "virtual@;"

let nonrec_flag f rf =
  match rf with Nonrecursive -> pp f "nonrec " | Recursive -> ()

let private_flag f = function Public -> () | Private -> pp f "private@ "
let constant_string f s = pp f "%S" s

let tyvar ppf s =
  if String.length s >= 2 && s.[1] = '\'' then Format.fprintf ppf "' %s" s
  else Format.fprintf ppf "'%s" s

let tyvar_loc f str = tyvar f str.txt

let rec class_params_def f = function
  | [] -> ()
  | l -> pp f "[%a] " (list type_param ~sep:",") l

and type_with_label f (label, c) =
  match label with
  | Nolabel -> core_type1 f c
  | Labelled s ->
      pp f
        "@[<2>{@;\
         \"kind\": \"type\",@;\
         \"variant\": \"labelled\",@;\
         \"label\": @,\
         %s,@;\
         \"type\": @,\
         %a@;\
         }@]"
        s core_type1 c
  | Optional s ->
      pp f
        "@[<2>{@;\
         \"kind\": \"type\",@;\
         \"variant\": \"optional\",@;\
         \"label\": @,\
         %s,@;\
         \"type\": @,\
         %a@;\
         }@]"
        s core_type1 c

and core_type f x =
  match x.ptyp_desc with
  | Ptyp_arrow (l, ct1, ct2) ->
      pp f
        "@[<2>{@;\
         \"kind\": \"type\",@;\
         \"variant\": \"arrow\",@;\
         \"left\": @,\
         %a,@;\
         \"right\": @,\
         %a@;\
         }@]"
        type_with_label (l, ct1) core_type ct2
  | Ptyp_alias (ct, s) ->
      pp f
        "@[<2>{@;\
         \"kind\": \"type\",@;\
         \"variant\": \"alias\",@;\
         \"type\": @,\
         %a,@;\
         \"alias\": @,\
         %a@;\
         }@]"
        core_type1 ct tyvar s
  | Ptyp_poly ([], ct) -> core_type f ct
  | Ptyp_poly (_, _) -> raise_not_implemented ()
  | _ -> core_type1 f x

and core_type1 f x =
  match x.ptyp_desc with
  | Ptyp_any -> pp f "@[<2>{@;\"kind\": \"type\",@;\"variant\": \"any\"@;}@]"
  | Ptyp_var s ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"type\",@;\
         \"variant\": \"variable\",@;\
         \"identifier\": \"%a\"@]@;\
         }@]"
        tyvar s
  | Ptyp_tuple l ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"type\",@;\
         \"variant\": \"tuple\",@;\
         \"items\": @,\
         [%a]@]@;\
         }@]"
        (list core_type1 ~sep:",@;")
        l
  | Ptyp_constr (li, l) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"type\",@;\
         \"variant\": \"constructed\",@;\
         \"constructor\": %a,@;\
         \"arguments\": [%a]@]@;\
         }@]"
        longident_loc li
        (fun f l ->
          match l with
          | [] -> ()
          | [ x ] -> pp f "%a@;" core_type1 x
          | _ -> list ~first:"(" ~last:")@;" core_type ~sep:",@;" f l)
        l
  | Ptyp_variant _ -> raise_not_implemented ()
  | Ptyp_object _ -> raise_not_implemented ()
  | Ptyp_class _ -> raise_not_implemented ()
  | Ptyp_package _ -> raise_not_implemented ()
  | Ptyp_extension _ -> raise_not_implemented ()
  | _ -> core_type f x

and pattern f x =
  match x.ppat_desc with
  | Ppat_alias (p, s) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"alias\",@;\
         \"pattern\": @,\
         %a,@;\
         \"alias\": \"%a\"@]@;\
         }@]"
        pattern p protect_ident s.txt
  | _ -> pattern_or f x

and pattern_or f x =
  let rec left_associative x acc =
    match x with
    | { ppat_desc = Ppat_or (p1, p2); ppat_attributes = []; _ } ->
        left_associative p1 (p2 :: acc)
    | x -> x :: acc
  in
  match left_associative x [] with
  | [] -> assert false
  | [ x ] -> pattern1 f x
  | orpats ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"or-pattern\",@;\
         \"patterns\": @,\
         [%a]@]@;\
         }@]"
        (list ~sep:",@ " pattern1) orpats

and pattern1 (f : Format.formatter) (x : pattern) : unit =
  match x.ppat_desc with
  | Ppat_variant (l, Some p) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"polymorphic-variant-pattern\",@;\
         \"tag\": %s,@,\
         \"argument\": @,\
         %a@]@;\
         }@]"
        l simple_pattern p
  | Ppat_construct (li, None) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"constructor-pattern\",@;\
         \"identifier\": @,\
         %a@]@;\
         }@]"
        longident_loc li
  | Ppat_construct (li, Some ([], po)) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"constructor-pattern\",@;\
         \"identifier\": @,\
         %a,@;\
         \"argument\": @,\
         %a@]@;\
         }@]"
        longident_loc li pattern po
  | Ppat_construct (_, Some (_, _)) -> raise_not_implemented ()
  | _ -> simple_pattern f x

and simple_pattern (f : Format.formatter) (x : pattern) : unit =
  match x.ppat_desc with
  | Ppat_construct _ -> pattern1 f x
  | Ppat_any ->
      pp f
        "@[<v 0>{@;<0 2>@[\"kind\": \"pattern\",@;\"variant\": \"any\"@]@;}@]"
  | Ppat_var { txt; _ } ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"variable-pattern\",@;\
         \"identifier\": \"%a\"@]@;\
         }@]"
        protect_ident txt
  | Ppat_array _ -> raise_not_implemented ()
  | Ppat_unpack _ -> raise_not_implemented ()
  | Ppat_type _ -> raise_not_implemented ()
  | Ppat_record (l, Closed) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[@\"kind\": \"pattern\",@;\
         \"variant\": \"record\",@;\
         \"closed\": true,@;\
         \"record\":@,\
         [%a]@]@;\
         }@]"
        (list ~sep:",@;" (fun f (l, p) ->
             pp f "@[<v 0>{@;<0 2>@[@\"label\":@,%a,@;\"pattern\":@,%a@]@;}@]"
               longident_loc l pattern p))
        l
  | Ppat_record (l, Open) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[@\"kind\": \"pattern\",@;\
         \"variant\": \"record\",@;\
         \"closed\": false,@;\
         \"record\":@,\
         [%a]@]@;\
         }@]"
        (list ~sep:",@;" (fun f (l, p) ->
             pp f "@[<v 0>{@;<0 2>@[@\"label\":@,%a,@;\"pattern\":@,%a@]@;}@]"
               longident_loc l pattern p))
        l
  | Ppat_tuple l ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"tuple\",@;\
         \"patterns\": [%a]@]@;\
         }@]"
        (list ~sep:",@;" pattern1) l
  | Ppat_constant c ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"constant\",@;\
         \"value\": %a@]@;\
         }@]"
        constant c
  | Ppat_interval (c1, c2) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"interval\",@;\
         \"left\": %a,@;\
         \"right\": %a@]@;\
         }@]"
        constant c1 constant c2
  | Ppat_variant _ -> raise_not_implemented ()
  | Ppat_constraint (p, ct) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"pattern\",@;\
         \"variant\": \"constraint\",@;\
         \"left\": %a,@;\
         \"right\": %a@]@;\
         }@]"
        pattern1 p core_type ct
  | Ppat_lazy _ -> raise_not_implemented ()
  | Ppat_exception _ -> raise_not_implemented ()
  | Ppat_extension _ -> raise_not_implemented ()
  | Ppat_open _ -> raise_not_implemented ()
  | _ -> pattern f x

and label_exp f (l, opt, p) =
  match l with
  | Nolabel ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"parameter\",@;\
         \"variant\": \"base\",@;\
         \"pattern\": @,\
         %a@]@;\
         }@]"
        simple_pattern p
  | Optional rest -> (
      match p with
      | { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = []; _ }
        when txt = rest -> (
          match opt with
          | Some o ->
              pp f
                "@[<v 0>{@;\
                 <0 2>@[\"kind\": \"parameter\",@;\
                 \"variant\": \"optional\",@;\
                 \"label\": @,\
                 %s,@;\
                 \"default\": @,\
                 %a@]@;\
                 }@]"
                rest expression o
          | None ->
              pp f
                "@[<v 0>{@;\
                 <0 2>@[\"kind\": \"parameter\",@;\
                 \"variant\": \"optional\",@;\
                 \"label\": @,\
                 %s@]@;\
                 }@]"
                rest)
      | _ -> (
          match opt with
          | Some o ->
              pp f
                "@[<v 0>{@;\
                 <0 2>@[\"kind\": \"parameter\",@;\
                 \"variant\": \"optional\",@;\
                 \"label\": @,\
                 %s,@;\
                 \"pattern\": @,\
                 %a,@;\
                 \"default\": @,\
                 %a@]@;\
                 }@]"
                rest pattern1 p expression o
          | None ->
              pp f
                "@[<v 0>{@;\
                 <0 2>@[\"kind\": \"parameter\",@;\
                 \"variant\": \"optional\",@;\
                 \"label\": @,\
                 %s,@;\
                 \"pattern\": @,\
                 %a@]@;\
                 }@]"
                rest simple_pattern p))
  | Labelled l -> (
      match p with
      | { ppat_desc = Ppat_var { txt; _ }; ppat_attributes = []; _ }
        when txt = l ->
          pp f
            "@[<v 0>{@;\
             <0 2>@[\"kind\": \"parameter\",@;\
             \"variant\": \"labelled\",@;\
             \"label\": @,\
             %s@]@;\
             }@]"
            l
      | _ ->
          pp f
            "@[<v 0>{@;\
             <0 2>@[\"kind\": \"parameter\",@;\
             \"variant\": \"labelled\",@;\
             \"label\": @,\
             %s,@;\
             \"pattern\": @,\
             %a@]@;\
             }@]"
            l simple_pattern p)

and expression f x =
  match x.pexp_desc with
  | Pexp_fun (l, e0, p, e) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"fun\",@;\
         \"parameter\": %a,@;\
         \"body\": @,\
         %a@]@;\
         }@]"
        label_exp (l, e0, p) expression e
  | Pexp_newtype _ -> raise_not_implemented ()
  | Pexp_function l ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"function\",@;\
         \"branches\": @,\
         [%a]@]@;\
         }@]"
        case_list l
  | Pexp_match (e, l) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"pattern-matching\",@;\
         \"matched-expression\": @,\
         %a,@;\
         \"branches\": @,\
         [%a]@]}@]"
        expression e case_list l
  | Pexp_try (e, l) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"try-with\",@;\
         \"body\": @,\
         %a,@;\
         \"catch\": @,\
         [%a]@]@;\
         }@]"
        expression e case_list l
  | Pexp_let (rf, l, e) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"let\",@;\
         \"bindings\": @,\
         [%a],@;\
         \"body\": @,\
         %a@]@;\
         }@]"
        bindings (rf, l) expression e
  | Pexp_apply (e, l) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"apply\",@;\
         \"head\": @,\
         %a,@;\
         \"arguments\": @,\
         [%a]@]@;\
         }@]"
        expression e
        (list ~sep:"," (fun f (label, exp) ->
             match label with
             | Nolabel ->
                 pp f
                   "@[<0>{@;\
                    <0 2>@[\"kind\": \"argument\",@;\
                    \"variant\": \"base\",@;\
                    \"expression\": @,\
                    %a@]@;\
                    }@]"
                   expression exp
             | Labelled s ->
                 pp f
                   "@[<0>{@;\
                    <0 2>@[\"kind\": \"argument\",@;\
                    \"variant\": \"labelled\",@;\
                    \"label\": \"%s\",@;\
                    \"expression\": @,\
                    %a@]@;\
                    }@]"
                   s expression exp
             | Optional s ->
                 pp f
                   "@[<0>{@;\
                    <0 2>@[\"kind\": \"argument\",@;\
                    \"variant\": \"optional\",@;\
                    \"label\": \"%s\",@;\
                    \"expression\": @,\
                    %a@]@;\
                    }@]"
                   s expression exp))
        l
  | Pexp_setfield (e1, li, e2) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"mutation\",@;\
         \"target\": @,\
         %a,@;\
         \"field\":\"%a\"@;\
         \"replacement\": @,\
         %a@]@;\
         }@]"
        simple_expr e1 longident_loc li simple_expr e2
  | Pexp_ifthenelse (e1, e2, None) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"if-then\",@;\
         \"condition\": @,\
         %a,@;\
         \"branch\": @,\
         %a@]@;\
         }@]"
        expression e1 expression e2
  | Pexp_ifthenelse (e1, e2, Some e3) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"if-then-else\",@;\
         \"condition\": @,\
         %a,@;\
         \"branch1\": @,\
         %a,@;\
         \"branch2\": @,\
         %a@]@;\
         }@]"
        expression e1 expression e2 expression e3
  | Pexp_sequence _ ->
      let rec sequence_helper acc = function
        | { pexp_desc = Pexp_sequence (e1, e2); pexp_attributes = []; _ } ->
            sequence_helper (e1 :: acc) e2
        | v -> List.rev (v :: acc)
      in
      let lst = sequence_helper [] x in
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"sequencing\",@;\
         \"statements\": @,\
         [%a]@]@;\
         }@]"
        (list expression ~sep:",@;")
        lst
  | Pexp_new _ -> raise_not_implemented ()
  | Pexp_setinstvar _ -> raise_not_implemented ()
  | Pexp_override _ -> raise_not_implemented ()
  | Pexp_letmodule _ -> raise_not_implemented ()
  | Pexp_letexception _ -> raise_not_implemented ()
  | Pexp_assert _ -> raise_not_implemented ()
  | Pexp_lazy _ -> raise_not_implemented ()
  | Pexp_poly _ -> raise_not_implemented ()
  | Pexp_open _ -> raise_not_implemented ()
  | Pexp_variant _ -> raise_not_implemented ()
  | Pexp_letop _ -> raise_not_implemented ()
  | Pexp_extension _ -> raise_not_implemented ()
  | Pexp_unreachable -> raise_not_implemented ()
  | _ -> expression1 f x

and expression1 f x =
  match x.pexp_desc with
  | Pexp_object _ -> raise_not_implemented ()
  | _ -> expression2 f x

and expression2 f x =
  match x.pexp_desc with
  | Pexp_field (e, li) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"field-access\",@;\
         \"record\": @,\
         %a,@;\
         \"field\": %a@]@;\
         }@]"
        simple_expr e longident_loc li
  | Pexp_send _ -> raise_not_implemented ()
  | _ -> simple_expr f x

and simple_expr f x =
  match x.pexp_desc with
  | Pexp_construct (ident, None) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"construct\",@;\
         \"constructor\": @,\
         %a@;\
         @]@;\
         }@]"
        longident_loc ident
  | Pexp_construct (ident, Some e) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"construct\",@;\
         \"constructor\": @,\
         %a,@;\
         \"argument\": @,\
         %a@;\
         @]@;\
         }@]"
        longident_loc ident expression e
  | Pexp_ident li ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"identifier\",@;\
         \"value\":@,\
         %a@;\
         }@]"
        longident_loc li
  | Pexp_constant c ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"constant\",@;\
         \"value\":@,\
         %a@;\
         }@]"
        constant c
  | Pexp_pack _ -> raise_not_implemented ()
  | Pexp_tuple l ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"tuple\",@;\
         \"items\":@,\
         [%a]@;\
         }@]"
        (list simple_expr ~sep:",@;")
        l
  | Pexp_constraint (e, ct) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"constraint\",@;\
         \"expression\":@,\
         %a,@;\
         \"constraint\":@,\
         %a@;\
         }@]"
        expression e core_type ct
  | Pexp_coerce (e, None, ct) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"coercion\",@;\
         \"expression\":@,\
         %a,@;\
         \"to\":@,\
         %a@;\
         }@]"
        expression e core_type ct
  | Pexp_coerce (e, Some cf, ct) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"coercion\",@;\
         \"expression\":@,\
         %a,@;\
         \"from\":@,\
         %a,@;\
         \"to\":@,\
         %a@;\
         }@]"
        expression e core_type cf core_type ct
  | Pexp_variant _ -> raise_not_implemented ()
  | Pexp_record (l, Some eo) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"record\",@;\
         \"original\":@,\
         %a,@;\
         \"fields\":@,\
         [%a]@;\
         }@]"
        expression eo
        (list ~sep:"," (fun f (l, e) ->
             pp f "@[<v 0>{@;<0 2>@[\"field\": %a,@;\"value\": %a@;}@]"
               longident_loc l expression e))
        l
  | Pexp_record (l, None) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"expression\",@;\
         \"variant\": \"record\",@;\
         \"fields\":@,\
         [%a]@;\
         }@]"
        (list ~sep:"," (fun f (l, e) ->
             pp f "@[<v 0>{@;<0 2>@[\"field\": %a,@;\"value\": %a@;}@]"
               longident_loc l expression e))
        l
  | Pexp_array _ -> raise_not_implemented ()
  | Pexp_while _ -> raise_not_implemented ()
  | Pexp_for _ -> raise_not_implemented ()
  | _ -> expression f x

and attributes f l = list ~sep:"@,\n" attribute f l
and item_attributes f l = List.iter (item_attribute f) l

and attribute f a =
  pp f "@[<2>[@@%s@ %a]@]" a.attr_name.txt payload a.attr_payload

and item_attribute f a =
  pp f "@[<2>[@@@@%s@ %a]@]" a.attr_name.txt payload a.attr_payload

and floating_attribute f a =
  pp f "@[<2>[@@@@@@%s@ %a]@]" a.attr_name.txt payload a.attr_payload

and value_description f x =
  pp f "@[<hov2>%a%a@]" core_type x.pval_type
    (fun f x ->
      if x.pval_prim <> [] then
        pp f "@ =@ %a" (list constant_string) x.pval_prim)
    x

and extension f (s, e) = pp f "@[<2>[%%%s@ %a]@]" s.txt payload e
and item_extension f (s, e) = pp f "@[<2>[%%%%%s@ %a]@]" s.txt payload e

and exception_declaration f x =
  pp f "@[<hov2>exception@ %a@]%a" extension_constructor x.ptyexn_constructor
    item_attributes x.ptyexn_attributes

and class_type _ _ = raise_not_implemented ()
and class_type_declaration_list _ _ = raise_not_implemented ()

and module_type f x =
  match x.pmty_desc with
  | Pmty_functor (Unit, mt2) ->
      pp f "@[<hov2>functor () ->@ %a@]" module_type mt2
  | Pmty_functor (Named (s, mt1), mt2) -> (
      match s.txt with
      | None -> pp f "@[<hov2>%a@ ->@ %a@]" module_type1 mt1 module_type mt2
      | Some name ->
          pp f "@[<hov2>functor@ (%s@ :@ %a)@ ->@ %a@]" name module_type mt1
            module_type mt2)
  | Pmty_with (mt, []) -> module_type f mt
  | Pmty_with (mt, l) ->
      pp f "@[<hov2>%a@ with@ %a@]" module_type1 mt
        (list with_constraint ~sep:"@ and@ ")
        l
  | _ -> module_type1 f x

and with_constraint f = function
  | Pwith_type (li, ({ ptype_params = ls; _ } as td)) ->
      let ls = List.map fst ls in
      pp f "type@ %a %a =@ %a"
        (list core_type ~sep:"," ~first:"(" ~last:")")
        ls longident_loc li type_declaration td
  | Pwith_module (li, li2) ->
      pp f "module %a =@ %a" longident_loc li longident_loc li2
  | Pwith_modtype (li, mty) ->
      pp f "module type %a =@ %a" longident_loc li module_type mty
  | Pwith_typesubst (li, ({ ptype_params = ls; _ } as td)) ->
      let ls = List.map fst ls in
      pp f "type@ %a %a :=@ %a"
        (list core_type ~sep:"," ~first:"(" ~last:")")
        ls longident_loc li type_declaration td
  | Pwith_modsubst (li, li2) ->
      pp f "module %a :=@ %a" longident_loc li longident_loc li2
  | Pwith_modtypesubst (li, mty) ->
      pp f "module type %a :=@ %a" longident_loc li module_type mty

and module_type1 f x =
  match x.pmty_desc with
  | Pmty_ident li -> pp f "%a" longident_loc li
  | Pmty_alias li -> pp f "(module %a)" longident_loc li
  | Pmty_signature s ->
      pp f "@[<hv0>@[<hv2>sig@ %a@]@ end@]" (list signature_item) s
  | Pmty_typeof me -> pp f "@[<hov2>module@ type@ of@ %a@]" module_expr me
  | Pmty_extension e -> extension f e
  | _ -> module_type f x

and signature f x = list ~sep:"@\n" signature_item f x

and signature_item f x : unit =
  match x.psig_desc with
  | Psig_type (rf, l) -> type_def_list f (rf, true, l)
  | Psig_typesubst l -> type_def_list f (Recursive, false, l)
  | Psig_value vd ->
      let intro = if vd.pval_prim = [] then "val" else "external" in
      pp f "@[<2>%s@ %a@ :@ %a@]%a" intro protect_ident vd.pval_name.txt
        value_description vd item_attributes vd.pval_attributes
  | Psig_typext te -> type_extension f te
  | Psig_exception ed -> exception_declaration f ed
  | Psig_class l -> (
      let class_description kwd f
          ({ pci_params = ls; pci_name = { txt; _ }; _ } as x) =
        pp f "@[<2>%s %a%a%s@;:@;%a@]%a" kwd virtual_flag x.pci_virt
          class_params_def ls txt class_type x.pci_expr item_attributes
          x.pci_attributes
      in
      match l with
      | [] -> ()
      | [ x ] -> class_description "class" f x
      | x :: xs ->
          pp f "@[<v>%a@,%a@]"
            (class_description "class")
            x
            (list ~sep:"@," (class_description "and"))
            xs)
  | Psig_module
      ({
         pmd_type = { pmty_desc = Pmty_alias alias; pmty_attributes = []; _ };
         _;
       } as pmd) ->
      pp f "@[<hov>module@ %s@ =@ %a@]%a"
        (Option.value pmd.pmd_name.txt ~default:"_")
        longident_loc alias item_attributes pmd.pmd_attributes
  | Psig_module pmd ->
      pp f "@[<hov>module@ %s@ :@ %a@]%a"
        (Option.value pmd.pmd_name.txt ~default:"_")
        module_type pmd.pmd_type item_attributes pmd.pmd_attributes
  | Psig_modsubst pms ->
      pp f "@[<hov>module@ %s@ :=@ %a@]%a" pms.pms_name.txt longident_loc
        pms.pms_manifest item_attributes pms.pms_attributes
  | Psig_open od ->
      pp f "@[<hov2>open%s@ %a@]%a"
        (override od.popen_override)
        longident_loc od.popen_expr item_attributes od.popen_attributes
  | Psig_include incl ->
      pp f "@[<hov2>include@ %a@]%a" module_type incl.pincl_mod item_attributes
        incl.pincl_attributes
  | Psig_modtype { pmtd_name = s; pmtd_type = md; pmtd_attributes = attrs; _ }
    ->
      pp f "@[<hov2>module@ type@ %s%a@]%a" s.txt
        (fun f md ->
          match md with
          | None -> ()
          | Some mt ->
              pp_print_space f ();
              pp f "@ =@ %a" module_type mt)
        md item_attributes attrs
  | Psig_modtypesubst
      { pmtd_name = s; pmtd_type = md; pmtd_attributes = attrs; _ } ->
      let md = match md with None -> assert false | Some mt -> mt in
      pp f "@[<hov2>module@ type@ %s@ :=@ %a@]%a" s.txt module_type md
        item_attributes attrs
  | Psig_class_type l -> class_type_declaration_list f l
  | Psig_recmodule decls ->
      let rec string_x_module_type_list f ?(first = true) l =
        match l with
        | [] -> ()
        | pmd :: tl ->
            if not first then
              pp f "@ @[<hov2>and@ %s:@ %a@]%a"
                (Option.value pmd.pmd_name.txt ~default:"_")
                module_type1 pmd.pmd_type item_attributes pmd.pmd_attributes
            else
              pp f "@[<hov2>module@ rec@ %s:@ %a@]%a"
                (Option.value pmd.pmd_name.txt ~default:"_")
                module_type1 pmd.pmd_type item_attributes pmd.pmd_attributes;
            string_x_module_type_list f ~first:false tl
      in
      string_x_module_type_list f decls
  | Psig_attribute a -> floating_attribute f a
  | Psig_extension (e, a) ->
      item_extension f e;
      item_attributes f a

and module_expr f x =
  match x.pmod_desc with
  | Pmod_structure s ->
      pp f "@[<hv2>struct@;@[<0>%a@]@;<1 -2>end@]"
        (list structure_item ~sep:"@\n")
        s
  | Pmod_constraint (me, mt) ->
      pp f "@[<hov2>(%a@ :@ %a)@]" module_expr me module_type mt
  | Pmod_ident li -> pp f "%a" longident_loc li
  | Pmod_functor (Unit, me) -> pp f "functor ()@;->@;%a" module_expr me
  | Pmod_functor (Named (s, mt), me) ->
      pp f "functor@ (%s@ :@ %a)@;->@;%a"
        (Option.value s.txt ~default:"_")
        module_type mt module_expr me
  | Pmod_apply (me1, me2) -> pp f "(%a)(%a)" module_expr me1 module_expr me2
  | Pmod_unpack e -> pp f "(val@ %a)" expression e
  | Pmod_extension e -> extension f e

and structure f x =
  pp f "@[<v 0>{@;<0 2>@[\"kind\": \"structure\",@;\"items\":@,[%a]@]@;}@]"
    (list ~sep:",@;" structure_item)
    x

and payload f = function
  | PStr [ { pstr_desc = Pstr_eval (e, attrs); _ } ] ->
      pp f "@[<2>%a@]%a" expression e item_attributes attrs
  | PStr x -> structure f x
  | PTyp x ->
      pp f ":@ ";
      core_type f x
  | PSig x ->
      pp f ":@ ";
      signature f x
  | PPat (x, None) ->
      pp f "?@ ";
      pattern f x
  | PPat (x, Some e) ->
      pp f "?@ ";
      pattern f x;
      pp f " when ";
      expression f e

and binding f { pvb_pat = p; pvb_expr = x; _ } =
  pp f
    "@[<2>{@;\
     \"kind\": \"value-binding\",@;\
     \"pattern\": @,\
     %a,@;\
     \"expression\": @,\
     %a@;\
     }@]"
    pattern p expression x

and bindings f (rf, l) =
  let binding rf f x =
    match rf with
    | Recursive ->
        pp f
          "@[<v 0>{@;\
           <0 2>@[\"kind\": \"value-bindings\",@;\
           \"recursive\": true,@;\
           \"bindings\": @,\
           [%a]@]@;\
           }@]"
          binding x
    | Nonrecursive ->
        pp f
          "@[<v 0>{@;\
           <0 2>@[\"kind\": \"value-bindings\",@;\
           \"recursive\": false,@;\
           \"bindings\": @,\
           [%a]@]@;\
           }@]"
          binding x
  in
  match l with
  | [] -> ()
  | [ x ] -> binding rf f x
  | x :: xs ->
      pp f "@[<v>%a,@,%a@]" (binding rf) x
        (list ~sep:",@," (binding Nonrecursive))
        xs

and structure_item f x =
  match x.pstr_desc with
  | Pstr_eval (e, attrs) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"structure-item\",@;\
         \"variant\": \"evaluation\",@;\
         \"expression\": @,\
         %a,@;\
         \"attributes\": @,\
         %a@]@;\
         }>@]"
        expression e item_attributes attrs
  | Pstr_type (_, []) -> assert false
  | Pstr_type (rf, l) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"structure-item\",@;\
         \"variant\": \"type-declarations\",@;\
         \"declarations\": @,\
         [%a]@]@;\
         }@]"
        type_def_list (rf, true, l)
  | Pstr_value (rf, l) ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"structure-item\",@;\
         \"variant\": \"value-bindings\",@;\
         \"bindings\": @,\
         [%a]@]@;\
         }@]"
        bindings (rf, l)
  | Pstr_typext _ -> raise_not_implemented ()
  | Pstr_exception ed ->
      pp f
        "@[<v 0>{@;\
         <0 2>@[\"kind\": \"structure-item\",@;\
         \"variant\": \"exception-declaration\",@;\
         \"declaration\": @,\
         %a@]@;\
         }@]"
        exception_declaration ed
  | Pstr_module x ->
      let rec module_helper = function
        | { pmod_desc = Pmod_functor (arg_opt, me'); pmod_attributes = []; _ }
          ->
            (match arg_opt with
            | Unit -> pp f "()"
            | Named (s, mt) ->
                pp f "(%s:%a)" (Option.value s.txt ~default:"_") module_type mt);
            module_helper me'
        | me -> me
      in
      pp f "@[<hov2>module %s%a@]%a"
        (Option.value x.pmb_name.txt ~default:"_")
        (fun f me ->
          let me = module_helper me in
          match me with
          | {
           pmod_desc =
             Pmod_constraint
               (me', ({ pmty_desc = Pmty_ident _ | Pmty_signature _; _ } as mt));
           pmod_attributes = [];
           _;
          } ->
              pp f " :@;%a@;=@;%a@;" module_type mt module_expr me'
          | _ -> pp f " =@ %a" module_expr me)
        x.pmb_expr item_attributes x.pmb_attributes
  | Pstr_open od ->
      pp f "@[<2>open%s@;%a@]%a"
        (override od.popen_override)
        module_expr od.popen_expr item_attributes od.popen_attributes
  | Pstr_modtype { pmtd_name = s; pmtd_type = md; pmtd_attributes = attrs; _ }
    ->
      pp f "@[<hov2>module@ type@ %s%a@]%a" s.txt
        (fun f md ->
          match md with
          | None -> ()
          | Some mt ->
              pp_print_space f ();
              pp f "@ =@ %a" module_type mt)
        md item_attributes attrs
  | Pstr_class _ -> raise_not_implemented ()
  | Pstr_class_type _ -> raise_not_implemented ()
  | Pstr_primitive _ -> raise_not_implemented ()
  | Pstr_include incl ->
      pp f "@[<hov2>include@ %a@]%a" module_expr incl.pincl_mod item_attributes
        incl.pincl_attributes
  | Pstr_recmodule decls -> (
      let aux f = function
        | { pmb_expr = { pmod_desc = Pmod_constraint (expr, typ); _ }; _ } as
          pmb ->
            pp f "@[<hov2>@ and@ %s:%a@ =@ %a@]%a"
              (Option.value pmb.pmb_name.txt ~default:"_")
              module_type typ module_expr expr item_attributes
              pmb.pmb_attributes
        | pmb ->
            pp f "@[<hov2>@ and@ %s@ =@ %a@]%a"
              (Option.value pmb.pmb_name.txt ~default:"_")
              module_expr pmb.pmb_expr item_attributes pmb.pmb_attributes
      in
      match decls with
      | ({ pmb_expr = { pmod_desc = Pmod_constraint (expr, typ); _ }; _ } as
        pmb)
        :: l2 ->
          pp f "@[<hv>@[<hov2>module@ rec@ %s:%a@ =@ %a@]%a@ %a@]"
            (Option.value pmb.pmb_name.txt ~default:"_")
            module_type typ module_expr expr item_attributes pmb.pmb_attributes
            (fun f l2 -> List.iter (aux f) l2)
            l2
      | pmb :: l2 ->
          pp f "@[<hv>@[<hov2>module@ rec@ %s@ =@ %a@]%a@ %a@]"
            (Option.value pmb.pmb_name.txt ~default:"_")
            module_expr pmb.pmb_expr item_attributes pmb.pmb_attributes
            (fun f l2 -> List.iter (aux f) l2)
            l2
      | _ -> assert false)
  | Pstr_attribute _ -> raise_not_implemented ()
  | Pstr_extension _ -> raise_not_implemented ()

and type_param f (ct, (a, b)) =
  pp f "%s%s%a" (type_variance a) (type_injectivity b) core_type ct

and type_params f = function
  | [] -> ()
  | l -> pp f "%a " (list type_param ~first:"(" ~last:")" ~sep:",@;") l

and type_def_list f (rf, exported, l) =
  let type_decl kwd rf f x =
    let eq =
      if x.ptype_kind = Ptype_abstract && x.ptype_manifest = None then ""
      else if exported then " ="
      else " :="
    in
    pp f "@[<2>%s %a%a%s%s%a@]%a" kwd nonrec_flag rf type_params x.ptype_params
      x.ptype_name.txt eq type_declaration x item_attributes x.ptype_attributes
  in
  match l with
  | [] -> assert false
  | [ x ] -> type_decl "type" rf f x
  | x :: xs ->
      pp f "@[<v>%a@,%a@]" (type_decl "type" rf) x
        (list ~sep:"@," (type_decl "and" Recursive))
        xs

and record_declaration f lbls =
  let type_record_field f pld =
    pp f "@[<2>%a%s:@;%a@;%a@]" mutable_flag pld.pld_mutable pld.pld_name.txt
      core_type pld.pld_type attributes pld.pld_attributes
  in
  pp f "{@\n%a}" (list type_record_field ~sep:";@\n") lbls

and type_declaration f x =
  let priv f =
    match x.ptype_private with Public -> () | Private -> pp f "@;private"
  in
  let manifest f =
    match x.ptype_manifest with
    | None -> ()
    | Some y ->
        if x.ptype_kind = Ptype_abstract then pp f "%t@;%a" priv core_type y
        else pp f "@;%a" core_type y
  in
  let constructor_declaration f pcd =
    pp f "|@;";
    constructor_declaration f
      ( pcd.pcd_name.txt,
        pcd.pcd_vars,
        pcd.pcd_args,
        pcd.pcd_res,
        pcd.pcd_attributes )
  in
  let repr f =
    let intro f = if x.ptype_manifest = None then () else pp f "@;=" in
    match x.ptype_kind with
    | Ptype_variant xs ->
        let variants fmt xs =
          if xs = [] then pp fmt " |"
          else pp fmt "@\n%a" (list ~sep:"@\n" constructor_declaration) xs
        in
        pp f "%t%t%a" intro priv variants xs
    | Ptype_abstract -> ()
    | Ptype_record l -> pp f "%t%t@;%a" intro priv record_declaration l
    | Ptype_open -> pp f "%t%t@;.." intro priv
  in
  let constraints f =
    List.iter
      (fun (ct1, ct2, _) ->
        pp f "@[<hov2>@ constraint@ %a@ =@ %a@]" core_type ct1 core_type ct2)
      x.ptype_cstrs
  in
  pp f "%t%t%t" manifest repr constraints

and type_extension f x =
  let extension_constructor f x = pp f "@\n|@;%a" extension_constructor x in
  pp f "@[<2>type %a%a += %a@ %a@]%a"
    (fun f -> function
      | [] -> ()
      | l -> pp f "%a@;" (list type_param ~first:"(" ~last:")" ~sep:",") l)
    x.ptyext_params longident_loc x.ptyext_path private_flag x.ptyext_private
    (list ~sep:"" extension_constructor)
    x.ptyext_constructors item_attributes x.ptyext_attributes

and constructor_declaration f (name, vars, args, res, attrs) =
  let name = match name with "::" -> "(::)" | s -> s in
  let pp_vars f vs =
    match vs with
    | [] -> ()
    | vs -> pp f "%a@;.@;" (list tyvar_loc ~sep:"@;") vs
  in
  match res with
  | None ->
      pp f "%s%a@;%a" name
        (fun f -> function
          | Pcstr_tuple [] -> ()
          | Pcstr_tuple l -> pp f "@;of@;%a" (list core_type1 ~sep:"@;*@;") l
          | Pcstr_record l -> pp f "@;of@;%a" record_declaration l)
        args attributes attrs
  | Some r ->
      pp f "%s:@;%a%a@;%a" name pp_vars vars
        (fun f -> function
          | Pcstr_tuple [] -> core_type1 f r
          | Pcstr_tuple l ->
              pp f "%a@;->@;%a" (list core_type1 ~sep:"@;*@;") l core_type1 r
          | Pcstr_record l ->
              pp f "%a@;->@;%a" record_declaration l core_type1 r)
        args attributes attrs

and extension_constructor f x =
  match x.pext_kind with
  | Pext_decl (v, l, r) ->
      constructor_declaration f (x.pext_name.txt, v, l, r, x.pext_attributes)
  | Pext_rebind li ->
      pp f "%s@;=@;%a%a" x.pext_name.txt longident_loc li attributes
        x.pext_attributes

and case_list f l : unit =
  let aux f = function
    | { pc_lhs; pc_guard = None; pc_rhs } ->
        pp f
          "@[<v 0>{@;\
           <0 2>@[\"kind\": \"pattern-matching-branch\",@;\
           \"pattern\": @,\
           %a,@;\
           \"branch\": @,\
           %a@]@;\
           }@]"
          pattern pc_lhs expression pc_rhs
    | { pc_lhs; pc_guard = Some pc_guard; pc_rhs } ->
        pp f
          "@[<v 0>{@;\
           <0 2>@[\"kind\": \"pattern-matching-branch\",@;\
           \"pattern\": @,\
           %a,@;\
           \"guard\": @,\
           %a,@;\
           \"branch\": @,\
           %a@]@;\
           }@]"
          pattern pc_lhs expression pc_guard expression pc_rhs
  in
  list aux f l ~sep:",@,"

let expression f x = pp f "@[%a@]" expression x
