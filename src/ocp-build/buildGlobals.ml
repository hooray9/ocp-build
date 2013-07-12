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

(* open OcpLang*)

(* open BuildBase *)
(* open Stdlib2 *)
open BuildTypes
open BuildOCPTree
open BuildOCPTypes
open BuildOCPVariable

let verbose = DebugVerbosity.verbose ["B"] "BuildGlobals"

(* Under Windows, we cannot use dot-prefixed directories *)
let homedir = try Sys.getenv "HOME" with Not_found -> "."

let time_arg = ref false
(*
let byte_arg = ref false
let asm_arg = ref false
*)
let clean_arg = ref false

let distclean_arg = ref false
let fake_arg = ref false
let save_config_arg = ref false

let stop_on_error_arg = ref true
let cross_arg = ref (Some "X" : string option)
let verbosity_arg = ref (None : int option)
let targets_arg = ref ([]: string list)
let distrib_arg = ref false
let conf_arg = ref false
let global_arg = ref false
let no_global_arg = ref false
let autogen_arg = ref false
let list_ocp_files = ref false


let packages_by_name =
  ref (StringMap.empty : BuildTypes.package_info StringMap.t)
let (all_projects : (int, BuildTypes.package_info) Hashtbl.t) = Hashtbl.create 111

let new_id_generator () =
  let counter = ref 0 in
  fun () ->
  let id = !counter in
  incr counter;
  id

let new_rule_id = new_id_generator ()
let new_file_id = new_id_generator ()
let new_dir_id = new_id_generator ()
let new_package_id = new_id_generator ()


(* For all projects ...
let (build_rules : (int, build_rule) Hashtbl.t) = Hashtbl.create 1111
let (build_files : (int, build_file) Hashtbl.t) = Hashtbl.create 1111
let (build_directories : (int * int,   build_directory) Hashtbl.t) = Hashtbl.create 1111
*)
(* let build_byte_targets = ref ([] : build_file list)
let build_asm_targets = ref ([] : build_file list) *)
(*
let get_project name = StringMap.find name !projects
*)


let new_library b pk package_dirname src_dir dst_dir mut_dir =

  let lib_installed = is_already_installed pk.package_options in
  let lib_install =
    not lib_installed &&
    (match pk.package_type with
        TestPackage -> false
      | ProgramPackage
      | LibraryPackage
      | ObjectsPackage
      | RulesPackage
      | SyntaxPackage -> true
    ) &&
    get_bool_with_default pk.package_options "install" true in

  let lib =
    {
      lib_context = b;
      lib_source_kind = pk.package_source_kind;
      lib_archive = get_string_with_default pk.package_options "archive" pk.package_name;
      lib_meta = get_bool_with_default pk.package_options "meta" false;
      lib_id = pk.package_id;
      lib_name = pk.package_name;
      lib_version = pk.package_version;
      lib_dirname = File.of_string package_dirname;
      lib_provides = pk.package_provides ;
      lib_type = pk.package_type ;
      lib_tag = pk.package_tag;
      lib_filename = pk.package_filename;
      lib_node = pk.package_node;
      lib_missing_deps = pk.package_missing_deps;
      lib_requires = List.map (fun dep ->
        let pd = try
(*          Printf.eprintf "Adding dep %d to %S (link = %b)\n%!"
            dep.dep_project.package_id pk.package_name dep.dep_link; *)
          Hashtbl.find all_projects dep.dep_project.package_id
        with Not_found ->
          Printf.eprintf "Unknown dependency %d of package %S\n%!"
            dep.dep_project.package_id pk.package_name;
          exit 2
          in
        { dep with dep_project = pd }
      ) pk.package_requires;
      lib_added = pk.package_added;
      lib_options = pk.package_options;

    (* lib_package = pj; *)
      lib_loc = (pk.package_filename, pk.package_loc, pk.package_name);
      lib_src_dir = src_dir;
      lib_dst_dir = dst_dir;
      lib_mut_dir = mut_dir;
      lib_byte_targets = [];
      lib_cmo_objects = [];
      lib_bytecomp_deps = [];
      lib_bytelink_deps = [];
      lib_asm_targets = [];
      lib_asm_cmx_objects = [];
      lib_asm_cmxo_objects = [];
      lib_asmcomp_deps = [];
      lib_asmlink_deps = [];
      lib_clink_deps = [];
      lib_modules = ref StringMap.empty;
      lib_internal_modules = StringsMap.empty;
      lib_dep_deps = IntMap.empty;
      lib_includes = None;
      lib_sources = pk.package_files;
      lib_tests = pk.package_tests;

      lib_build_targets = [];

      lib_installed;
      lib_install;

      lib_bundles = [];
    }
  in
  Hashtbl.add all_projects lib.lib_id lib;
  packages_by_name := StringMap.add lib.lib_name lib !packages_by_name;
  if verbose 5 then begin
    Printf.eprintf "BuildGlobals.new_library %S\n" lib.lib_name;
    Printf.eprintf "  lib_install = %b\n%!" lib.lib_install;
    List.iter (fun (s, _) ->
      Printf.eprintf "  MOD %S\n%!" s;
    ) lib.lib_sources;
  end;
  lib


let absolute_filename dirname =
  if Filename.is_relative dirname then
    Filename.concat (Unix.getcwd ()) dirname
  else dirname

let installed_files = ref []
let register_installed (file : string) =
  installed_files := file :: !installed_files

(* TODO
let register_project pk =
  Hashtbl.add all_projects pk.lib_id pk
*)


