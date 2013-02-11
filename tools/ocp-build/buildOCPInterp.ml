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

open BuildOCPVariable
open BuildOCPTree
open BuildOCPTypes

type config = {
  config_options : BuildOCPVariable.options;
  config_files : string_with_attributes list;
  config_tests : string_with_attributes list;
  config_requires : string_with_attributes list;
  config_dirname : string;
  config_filename : string;
}

type state = {
  mutable packages : package IntMap.t;
  mutable npackages : int;
}

let initial_state () =
{ packages = IntMap.empty; npackages = 0; }

let final_state state =
  if state.npackages = 0 then [||] else
    Array.init state.npackages (fun i ->
      IntMap.find i state.packages
    )

let new_package pj name dirname filename kind =
  let package_id = pj.npackages in
  pj.npackages <- pj.npackages + 1;
  let pk = {
    package_source_kind = "ocp";
    package_id = package_id;
    package_tag = "";
    package_auto = None;
    package_version = "";
    package_loc = (-1);
    package_filename = filename;
    package_node = LinearToposort.new_node ();
    package_added = false;
    package_requires = [];
    package_name = name;
    package_missing_deps = 0;
    package_provides = name;
    package_type = kind;
(*    package_native = true; *)
    package_validated = false;
    package_raw_files = [];
    package_raw_tests = [];
    package_files = [];
    package_tests = [];
    package_dirname = dirname;
    package_deps_map = StringMap.empty;
(*    package_deps_sorted = []; *)
    package_options = new_options;
(*    package_cflags = ""; *)
(*    package_cclib = ""; *)
(*    package_details = None; *)
(*    package_has_byte = true; *)
    package_has_byte_debug = false;
(*    package_has_asm = true; *)
    package_has_asm_debug = false;
    package_has_asm_profile = false;
  } in
  pj.packages <- IntMap.add pk.package_id pk pj.packages;
  pk

let empty_config set_defaults =
  let options = BuildOCPVariable.new_options in
  let options =
    List.fold_left (fun vars f -> f vars)
      options set_defaults
  in
  { config_options = options;
    config_files = [];
    config_tests = [];
    config_requires = [];
    config_dirname = "";
    config_filename = "";
  }


let configs = Hashtbl.create 17


let define_config config_name options = Hashtbl.add configs config_name options
let find_config config_name = Hashtbl.find configs config_name

let option_list_set options name list =
      StringMap.add name (OptionList list) options


let option_bool_set options name bool =
  StringMap.add name (OptionBool bool) options

let rec option_list_remove prev list =
  match list with
      [] -> prev
    | first_ele :: next_eles ->
      let rec iter prev before =
        match prev with
            [] -> List.rev before
          | x :: tail ->
            if x = first_ele then
              cut_after x tail tail next_eles before
            else
              iter tail (x :: before)

      and cut_after head tail list1 list2 before =
        match list1, list2 with
            _, [] -> iter list1 before
          | x1 :: tail1, x2 :: tail2 when x1 = x2 ->
            cut_after head tail tail1 tail2 before
          | _ -> iter tail (head :: before)
      in
      iter prev []

let option_list_remove options name list =
  try
    match StringMap.find name options with
	OptionList prev ->
          let new_list = option_list_remove prev list in
          StringMap.add name (OptionList new_list) options
      | _ ->
	failwith  (Printf.sprintf "OptionListRemove %s: incompatible values" name);
  with Not_found -> options



let option_list_append options name list =
  let list =
    try
      match StringMap.find name options with
	  OptionList prev -> prev @ list
	| _ ->
	  failwith  (Printf.sprintf "OptionListAppend %s: incompatible values" name);
    with Not_found -> list
  in
  StringMap.add name (OptionList list) options

let options_list_append options names list =
  List.fold_left (fun options var ->
    option_list_append options var list
  ) options names

let options_list_remove options names list =
  List.fold_left (fun options var ->
    option_list_remove options var list
  ) options names

let options_list_set options names list =
  List.fold_left (fun options var ->
    option_list_set options var list
  ) options names

let meta_options = [
  "o",      [ "dep"; "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "oc",      [ "bytecomp"; "bytelink"; "asmcomp"; "asmlink" ];
  "byte",   [        "bytecomp"; "bytelink"; ];
  "asm",   [        "asmcomp"; "asmlink"; ];
  "comp",   [        "bytecomp"; "asmcomp"; ];
  "link",   [        "bytelink"; "asmlink"; ];
]

let meta_options =
  let list = ref StringMap.empty in
  List.iter (fun (name, names) -> list := StringMap.add name names !list) meta_options;
  !list

let option_list_set options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_set options names list
  with Not_found -> option_list_set options name list

let option_list_append options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_append options names list
  with Not_found -> option_list_append options name list


let option_list_remove options name list =
  try
    let names = StringMap.find name meta_options in
    options_list_remove options names list
  with Not_found -> option_list_remove options name list


let generated_config set_defaults =
  let config = empty_config set_defaults in
  { config with config_options = option_bool_set config.config_options
    "generated" true }


let rec translate_condition options cond =
  match cond with
    | IsEqualStringList (name, list) ->
      let name_value = try
                         StringMap.find name options
        with Not_found -> OptionList []
      in
      (*      Printf.fprintf stderr "translate_condition %s = %S = %S\n%!"
	      name (match name_value with
	      OptionList name_value -> String.concat ";"  name_value
	      | _ -> "???" )
	      (String.concat ";" list); *)
      name_value = OptionList list
    | IsTrue name ->
      let name_value = try
                         StringMap.find name options
        with Not_found -> OptionBool false
      in
      name_value = OptionBool true
    | NotCondition cond -> not (translate_condition options cond)
    | AndConditions (cond1, cond2) ->
      (translate_condition options cond1) && (translate_condition options cond2)
    | OrConditions (cond1, cond2) ->
      (translate_condition options cond1) || (translate_condition options cond2)

let rec translate_options options list =
  match list with
      [] -> options
    | option :: list ->
      let options = translate_option options option in
      translate_options options list

and translate_option options op =
  match op with
    | OptionConfigSet config_name ->
      find_config config_name options

    | OptionListSet (name, list) -> option_list_set options name list

    | OptionListAppend (name, list) -> option_list_append options name list
    | OptionListRemove (name, list) -> option_list_remove options name list

    | OptionBoolSet (name, bool) ->
      StringMap.add name (OptionBool bool) options

    | OptionIfThenElse (cond, ifthen, ifelse) ->
      begin
        if translate_condition options cond then
          translate_options options ifthen
        else
          match ifelse with
              None -> options
            | Some ifelse ->
              translate_options options ifelse
      end
    | OptionBlock list -> translate_options options list

(*
  module MakeParser(BuildGlobals : sig

  type project_info

  val new_project_id : unit -> int
  val register_project : project_info  BuildOCPTypes.project -> unit
  val new_project :
(* project name *) string ->
(* project dirname *) string ->
(* configuration filename *) string ->
  project_info BuildOCPTypes.project

  val register_installed : string -> unit

  end) = struct
*)


let new_package_dep pk s =
    try
      StringMap.find s pk.package_deps_map
    with Not_found ->
      let dep = {
        dep_project = s;
        dep_link = false;
        dep_syntax = false;
        dep_optional = false;
      }
      in
      pk.package_deps_map <- StringMap.add s dep pk.package_deps_map;
      dep

let add_project_dep pk s options =
  let dep = new_package_dep pk s in
  begin
    try
      match StringMap.find "link" options with
        OptionBool bool ->
          dep.dep_link <- bool
      | _ ->
        Printf.fprintf stderr "Warning: option \"link\" is not bool !\n%!";
    with Not_found -> ()
  end;

(*
  begin
    try
      match StringMap.find "syntax" options with
        OptionBool bool -> dep.dep_syntax <- bool
      | _ ->
        Printf.fprintf stderr "Warning: option \"syntax\" is not bool !\n%!";
    with Not_found -> ()
  end;
*)

  begin
    try
      match StringMap.find "optional" options with
        OptionBool bool -> dep.dep_optional <- bool
      | _ ->
        Printf.fprintf stderr "Warning: option \"optional\" is not bool !\n%!";
    with Not_found -> ()
  end;
(*  Printf.eprintf "add_project_dep for %S = %S with link=%b\n%!"
    pk.package_name s dep.dep_link; *)
()

let define_package pj name config kind =
  let dirname =
    try
      match StringMap.find "dirname" config.config_options with
	  OptionList list ->
            BuildSubst.subst_env (String.concat Filename.dir_sep list)
	| _ -> raise Not_found
      with Not_found ->
        config.config_dirname
  in

  let pk = new_package pj name
    dirname config.config_filename kind
  in
  let project_options = config.config_options in
  pk.package_raw_files <-  config.config_files;
  pk.package_raw_tests <- config.config_tests;
  pk.package_options <- project_options;
  pk.package_version <- string_option_with_default project_options "version"
    "0.1-alpha";
  List.iter (fun (s, options) ->
    let options = translate_options default_options options in
    add_project_dep pk s options
  ) config.config_requires


let rec translate_toplevel_statements pj config list =
  match list with
      [] -> config
    | stmt :: list ->
      let config = translate_toplevel_statement pj config stmt in
      translate_toplevel_statements pj config list

and translate_toplevel_statement pj config stmt =
  match stmt with
    | StmtDefineConfig (config_name, options) ->
      begin
        (*	      let options = translate_options config.config_options options in *)
	define_config config_name (fun old_options -> translate_options old_options options);
      end;
      config
    | StmtDefinePackage (package_type, library_name, simple_statements) ->
      begin
	let config = translate_statements pj config simple_statements in
	define_package pj library_name config package_type
      end;
      config
    | StmtBlock statements ->
      ignore (translate_toplevel_statements pj config statements);
      config
    | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition config.config_options cond then
        translate_toplevel_statements pj config ifthen
      else
        match ifelse with
            None -> config
          | Some ifelse ->
            translate_toplevel_statements pj config ifelse
    end
    | _ -> translate_simple_statement pj config stmt

and translate_statements pj config list =
  match list with
      [] -> config
    | stmt :: list ->
      let config = translate_statement pj config stmt in
      translate_statements pj config list

and translate_statement pj config stmt =
  match stmt with
    | StmtIfThenElse (cond, ifthen, ifelse) -> begin
      if translate_condition config.config_options cond then
        translate_statements pj config ifthen
      else
        match ifelse with
            None -> config
          | Some ifelse ->
            translate_statements pj config ifelse
    end
    | _ -> translate_simple_statement pj config stmt

and translate_simple_statement pj config stmt =
  match stmt with
    | StmtRequiresSet requirements ->
      { config with config_requires = requirements }
    | StmtRequiresAppend requirements ->
      { config with config_requires = config.config_requires @ requirements }
    | StmtFilesSet files ->
      { config with config_files = files }
    | StmtFilesAppend files ->
      { config with config_files = config.config_files @ files }
    | StmtTestsSet files ->
      { config with config_tests = files }
    | StmtTestsAppend files ->
      { config with config_tests = config.config_tests @ files }
    | StmtOption option ->
      { config with config_options = translate_option config.config_options option }
    | StmtSyntax (syntax_name, camlpN, extensions) -> config
    | StmtIfThenElse _
    | StmtBlock _
    | StmtDefinePackage _
    | StmtDefineConfig _ -> assert false

let read_ocamlconf pj config filename =
  let ast = BuildOCPParse.read_ocamlconf filename in
  translate_toplevel_statements pj
    { config
      with
	config_files = [];
        config_tests = [];
	config_requires = [];
	config_dirname = Filename.dirname filename;
	config_filename = filename;
    } ast
