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

(* ocp-build install [OPTIONS]

  Set the options of the user preference file.

*)

(* open BuildBase *)
open BuildArgs
open BuildOptions
(* open Stdlib2 *)
open SimpleConfig

open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
open BuildArgs
open BuildTerm
open BuildActions


let query_libdir = ref []
let query_package = ref []

let arg_list =
  BuildOptions.merge
    [
      [
        "-libdir", Arg.String (fun s ->
          query_libdir := s :: !query_libdir
        ),
        "PACKAGE Query libdir of PACKAGE";
        "-has", Arg.String (fun s ->
          query_package := s :: !query_package
        ),
        "PACKAGE Query if PACKAGE is available";

      ];
      BuildActionBuild.arg_list
    ]



let do_reply_to_queries pj =

  List.iter (fun p ->
    Array.iter (fun pk ->
      if pk.package_name = p then begin
        Printf.printf "%s\n" pk.package_dirname;
        exit 0
      end
    ) pj.project_sorted;
    Printf.eprintf "Error: no package %S\n%!" p;
    exit 2
  ) !query_libdir;

  List.iter (fun p ->
    try
      Array.iter (fun pk ->
        if pk.package_name = p then raise Exit
      ) pj.project_sorted;
      Printf.eprintf "Error: no package %S\n%!" p;
      exit 2
    with Exit ->
      Printf.printf "Package %S is present\n%!" p
  ) !query_package

let action () =
  let p = BuildActions.load_project () in
  let state = BuildActionBuild.do_read_env p in

    time_step "Sorting packages...";
    let pj = BuildOCP.verify_packages state in

    time_step "   Done sorting packages";

    do_reply_to_queries pj;

  ()



let subcommand = {
  sub_name = "query";
  sub_help =  "Install the project.";
  sub_arg_list = arg_list;
  sub_arg_anon = None;
  sub_arg_usage = [ "Query information about environment."; ];
  sub_action = action;
}

