(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

(* open OcpLang *)
(* open BuildEngineTypes *)

type camlpN =
  | Camlp4
  | Camlp5

type package_type =
  | ProgramPackage
  | TestPackage
  | LibraryPackage
  | ObjectsPackage
  | SyntaxPackage
  | RulesPackage

and condition =
  | IsEqual of expression * expression
  | IsNonFalse of expression
  | NotCondition of condition
  | AndConditions of condition * condition
  | OrConditions of condition * condition

and expression = simple_expression list

and simple_expression = value * set_option_list

and set_option_list = set_option list

and value =
  | ValueString of string
  | ValueVariable of string
  | ValuePrimitive of string

and set_option =
  | OptionVariableSet of string * expression
  | OptionVariableAppend of string * expression
  | OptionConfigUse of string
  | OptionIfThenElse of condition * set_option * set_option option
  | OptionBlock of set_option list
(*  | OptionConfigAppend of string *)

type statement =
    StmtOption of set_option
  | StmtBlock of statement list
  | StmtDefineConfig of string * set_option list
  | StmtDefinePackage of package_type * string * statement list
  | StmtIfThenElse of condition * statement list * statement list option

let modname_of_fullname fullname =
  let modname = Filename.chop_extension (Filename.basename fullname) in
  modname.[0] <- Char.uppercase modname.[0];
  modname

let string_of_package_type kind =
  match kind with
    ProgramPackage -> "program"
  | LibraryPackage -> "library"
  | SyntaxPackage -> "syntax"
          (*	  | ProjectToplevel -> "toplevel" *)
  | ObjectsPackage -> "objects"
  | TestPackage -> "test"
  | RulesPackage -> "rules"


let rec string_of_condition cond =
  match cond with
  | IsEqual (exp1, exp2) ->
    Printf.sprintf "%s = %s"
      (string_of_expression exp1) (string_of_expression exp2)
  | IsNonFalse exp ->
    Printf.sprintf "%s" (string_of_expression exp)
  | NotCondition cond ->
    Printf.sprintf "not ( %s )" (string_of_condition cond)
  | AndConditions (cond1, cond2) ->
    Printf.sprintf "( %s ) && ( %s )" (string_of_condition cond1) (string_of_condition cond2)
  | OrConditions (cond1, cond2) ->
    Printf.sprintf "( %s ) || ( %s )" (string_of_condition cond1) (string_of_condition cond2)

and string_of_expression exp =
  match exp with
  | [ ValueString "true", [] ] -> "true"
  | [] -> "[]"
  | _ -> Printf.sprintf "[ %s ]"
           (String.concat "; " (List.map string_of_simple_expression exp))

and string_of_simple_expression (v, options) =
  Printf.sprintf "%s%s"
    (match v with
     | ValueString s -> Printf.sprintf "%S" s
     | ValueVariable s -> Printf.sprintf "%s" s
     | ValuePrimitive s -> Printf.sprintf "%%%s" s
    )
    (match options with
     | [] -> ""
     | _ -> Printf.sprintf "(%s)" (String.concat ";" (List.map string_of_set_option options)))

and string_of_set_option option =
  match option with
  | OptionVariableSet (v, exp) ->
    Printf.sprintf "%s = %s" v (string_of_expression exp)
  | OptionVariableAppend (v, exp) ->
    Printf.sprintf "%s += %s" v (string_of_expression exp)
  | OptionConfigUse c -> Printf.sprintf "use \"%s\"" c
  | OptionIfThenElse (cond, ifthen, ifelse) ->
    Printf.sprintf "if %s then %s%s"
      (string_of_condition cond)
      (string_of_set_option ifthen)
      (match ifelse with None -> ""
                       | Some ifelse -> Printf.sprintf " else %s"
                                          (string_of_set_option ifelse))
  | OptionBlock options ->
    Printf.sprintf "begin\n%s\nend"
      (String.concat "\n" (List.map string_of_set_option options))
