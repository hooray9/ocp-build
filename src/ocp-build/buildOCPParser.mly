/******************************************************************************/
/*                                                                            */
/*                          TypeRex OCaml Tools                               */
/*                                                                            */
/*                               OCamlPro                                     */
/*                                                                            */
/*    Copyright 2011-2012 OCamlPro                                            */
/*    All rights reserved.  See accompanying files for the terms under        */
/*    which this file is distributed. In doubt, contact us at                 */
/*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         */
/*                                                                            */
/******************************************************************************/

%{

open BuildOCPTree
  (* TODO: location of the type in ocamlyacc is erroneous, for example here token "main"
   type is located in the .mli/.ml file instead of the .mly file. *)

%}

%token <string> STRING
%token <int> INT
%token EOF
%token <float> FLOAT
%token <char> CHAR
%token SEMI
%token BEGIN
%token END
%token <string> IDENT
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token PLUSEQUAL
%token MINUSEQUAL
%token TRUE
%token FALSE
%token INCLUDE
%token <BuildOCPTree.statement list> INCLUDED
%token OBJECTS
%token LIBRARY
%token SYNTAX
%token PROGRAM
%token CONFIG
%token RULES
%token BANG
%token EQUAL
%token LPAREN
%token RPAREN
%token TYPE
%token USE
%token PACK
%token IF
%token THEN
%token ELSE
%token NOT
%token COND_OR
%token COND_AND
%token SYNTAXES
%token CAMLP4
%token CAMLP5
%token TEST
%token PERCENT
/* %token TESTS */

%start main
%type <BuildOCPTree.statement list> main

%%

main:
toplevel_statements EOF { $1 }
;

toplevel_statements:
  { [] }
| INCLUDED toplevel_statements { $1 @ $2 }
| toplevel_statement toplevel_statements { $1 :: $2 }
| SEMI toplevel_statements { $2 }
;

package_type:
  PROGRAM { ProgramPackage }
| LIBRARY { LibraryPackage }
| TEST    { TestPackage }
| OBJECTS { ObjectsPackage }
| SYNTAX  { SyntaxPackage }
| RULES   { RulesPackage }
;

toplevel_statement:
| BEGIN CONFIG STRING list_of_set_options END { StmtDefineConfig ($3, $4) }
| BEGIN package_type STRING statements END { StmtDefinePackage ($2, $3, $4) }
| BEGIN toplevel_statements END { StmtBlock $2 }
| IF condition THEN one_toplevel_statement maybe_else_one_toplevel_statement { StmtIfThenElse($2,$4,$5) }
| simple_statement { $1 }

/* for backward compatibility
| BEGIN STRING TYPE EQUAL package_type statements END { StmtDefinePackage ($5, $2, $6) }
*/
;

one_toplevel_statement:
| toplevel_statement { [$1] }
| LBRACE toplevel_statements RBRACE { $2 }
;

statements:
| statement statements { $1 :: $2 }
| { [] }
| SEMI statements { $2 }
;

statement:
| BEGIN statements END { StmtBlock $2 }
| IF condition THEN one_statement maybe_else_one_statement { StmtIfThenElse($2, $4, $5) }
| simple_statement { $1 }
;

one_statement:
| statement { [$1] }
| LBRACE statements RBRACE { $2 }
;

simple_statement:
| set_option { StmtOption $1 }
;

maybe_else_one_statement:
| { None }
| ELSE one_statement { Some $2 }
;

maybe_else_one_toplevel_statement:
| { None }
| ELSE one_toplevel_statement { Some $2 }
;

condition:
condition2 { $1 }
;

condition2:
| condition1 COND_OR condition2 { OrConditions ($1, $3) }
| condition1 { $1 }
;

condition1:
| condition0 COND_AND condition1 { AndConditions ($1, $3) }
| condition0 { $1 }
;

condition0:
| NOT condition0 { NotCondition $2 }
| LPAREN condition RPAREN { $2 }
| expression EQUAL expression { IsEqual($1, $3) }
| expression { IsNonFalse $1 }
;

expression:
| simple_expression { [ $1 ] }
| LBRACKET list_of_simple_expressions RBRACKET { $2 }
;

list_of_simple_expressions:
| { [] }
| SEMI list_of_simple_expressions { $2 }
| simple_expression list_of_simple_expressions { $1 :: $2 }
| BEGIN set_option_list list_of_simple_expressions END set_option_list list_of_simple_expressions
   { let begin_options = $2 in
     let inner_files = $3 in
     let end_options = $5 in
     let outter_files = $6 in
     let inner_files =
       List.map (fun (file, file_options) ->
	 (file, begin_options @ file_options @ end_options)
       ) inner_files in
     inner_files @ outter_files
   }
| packer set_option_list expression set_option_list list_of_simple_expressions {
  let packname = $1 in
  let pack_options1 = $2 in
  let files = $3 in
  let pack_options2 = $4 in
  let other_files = $5 in
  let pack_options = pack_options1 @ pack_options2 in

  let packmodname = BuildOCPTree.modname_of_fullname packname in

  let modnames = ref [] in
  let packed_files =
    List.map (fun (file, file_options) ->
      match file with
      | ValueVariable _ -> assert false (* TODO *)
      | ValuePrimitive _ -> assert false (* TODO *)
      | ValueString file ->
      if not (List.exists (function
            OptionVariableAppend ( "packed", _ )  -> true
          | _ -> false
          ) file_options) then
        modnames := Filename.basename file :: !modnames;
      (ValueString file, OptionVariableAppend ("packed", [ ValueString packmodname, [] ]) ::
         pack_options @ file_options)
  ) files;
  in
  packed_files @
    [ ValueString packname,
      OptionVariableSet ("pack",
        List.map (fun s -> ValueString s,[]) (List.rev !modnames)) :: pack_options] @
    other_files
}
;

simple_expression:
| PERCENT IDENT set_option_list { ValuePrimitive $2, $3 }
| IDENT set_option_list { ValueVariable $1, $2 }
| STRING  set_option_list { ValueString   $1, $2 }
| INT     set_option_list { ValueString (string_of_int  $1), $2 }
| LBRACE list_of_simple_expressions RBRACE set_option_list
  { ValueString "", (OptionVariableSet ("value", $2)) :: $4 }
;

set_option_list:
|   { [] }
| LPAREN list_of_set_options RPAREN { $2 }
;

list_of_set_options:
| { [] }
| SEMI list_of_set_options { $2 }
| set_option list_of_set_options { $1 :: $2 }
;

set_option:
| USE STRING { OptionConfigUse $2 }
| ident EQUAL expression { OptionVariableSet ($1,$3) }
| ident PLUSEQUAL expression { OptionVariableAppend ($1,$3) }
| ident EQUAL TRUE { OptionVariableSet ($1, [ ValueString "true", [] ]) }
| ident EQUAL FALSE { OptionVariableSet ($1, []) }
| IF condition THEN one_set_option maybe_else_one_set_option { OptionIfThenElse($2, $4, $5) }
;

ident:
| STRING { $1 }
| IDENT  { $1 }
| SYNTAX { "syntax" }
| RULES  { "rules" }
;

maybe_else_one_set_option:
| { None }
| ELSE one_set_option { Some $2 }
;

one_set_option:
| set_option { $1  }
| LBRACE list_of_set_options RBRACE { OptionBlock $2 }
;

packer:
| PACK STRING { $2 }
| PACK IDENT  { let s = $2 in s.[0] <- Char.lowercase s.[0]; s ^ ".ml" }
;

%%
