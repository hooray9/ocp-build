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
open BuildEngineTypes
open BuildTypes
open BuildOCamlVariables
open BuildOCPTree
open BuildOCPTypes

let verbose = DebugVerbosity.verbose ["B"] "BuildOCamlTest"

type stats = {
  mutable tests_nsuccesses : int;
  mutable tests_nfailures : int;
  mutable tests_failures : (BuildTypes.package_info * string * string) list;
}

let init () =
  {
    tests_nsuccesses = 0;
    tests_nfailures = 0;
    tests_failures = [];
  }

let find_binary b lib =
  let has_asm = bool_option_true lib.lib_options asm_option in
  let _has_byte = bool_option_true lib.lib_options byte_option in
  (File.to_string
     (File.add_basenames b.build_dir
        [ lib.lib_name;
          lib.lib_name ^ (if has_asm then ".asm" else ".byte")]))

let test_package b stats lib =
  let cwd = Unix.getcwd () in
  try
    let binary, tests =
      match lib.lib_type, lib.lib_tests with
      | TestPackage, _ ->
        let binary =
        if lib.lib_sources = [] then
          let program = ref None in
          List.iter (fun dep ->
            let pro = dep.dep_project in
            match pro.lib_type, !program with
            | ProgramPackage, Some _ ->
              Printf.eprintf "Error: test %S depends on two programs\n%!"
                lib.lib_name;
              exit 2
            | ProgramPackage, None ->
              program := Some pro
            | _ -> () (* TODO: raise an error ? *)
          ) lib.lib_requires;
          begin
            match !program with
              None ->
              Printf.eprintf "Error: test %S has no source files.\n%!"
                lib.lib_name;
              Printf.eprintf "  It should depend on the program it is testing.";
              exit 2
            | Some pro -> find_binary b pro
          end
        else
          find_binary b lib
        in
        let tests =
          match lib.lib_tests with
            [] -> [ "default", lib.lib_options ]
          | tests -> tests
        in
        binary, tests

      | ProgramPackage, tests ->
        find_binary b lib, tests
      | (ObjectsPackage
        | LibraryPackage
        | SyntaxPackage), _
        -> assert false
    in
    if tests <> [] then
    begin

      if verbose 1 then
          Printf.eprintf "%d tests to run for test %S...\n%!"
          (List.length tests) lib.lib_name;

      let tests_dir = File.add_basenames b.build_dir
          [ lib.lib_name; "tests" ]
      in
      List.iter (fun (test, options) ->
        let s = BuildSubst.env_subst in
        let s = StringSubst.add_to_copy s "%{test}%" test in
        let s = StringSubst.add_to_copy s "%{binary}%" binary in
        let s = StringSubst.add_to_copy s "%{tests}%"
            (File.to_string
               (File.add_basename lib.lib_src_dir.dir_file "tests"))
        in
        let test_dir = File.add_basename tests_dir test in
        BuildMisc.safe_mkdir (File.to_string test_dir);
        let cmd = list_option_with_default options
            "test_cmd" [ "%{binary}%" ]
        in
        let cmd_args = list_option_with_default options
            "test_args" []
        in
        let cmd_args = cmd @ cmd_args in
        let cmd_args = List.map (BuildSubst.subst s) cmd_args in
        if verbose 2 then
          Printf.eprintf "Starting test '%s'\n%!"
            (String.concat "' '" cmd_args);
        let result_out =
          File.to_string (File.add_basename test_dir "result.out")
        in
        let pid = BuildMisc.create_process cmd_args
            (Some result_out)
            (Some (File.to_string (File.add_basename test_dir "result.err")))
        in
        if verbose 2 then
          Printf.eprintf "Test started. Waiting...\n%!";
        let status = BuildMisc.wait_command pid in
        if verbose 2 then
          Printf.eprintf "Test finished\n%!";
        if status = 0 then begin
          let output = list_option_with_default options
              "test_output" []
          in
          if output = [] then
            stats.tests_nsuccesses <- stats.tests_nsuccesses + 1
          else begin try
            let cmd_output = try
              File.string_of_file result_out
            with _ -> failwith "Missing output"
            in
            let expected_output_file =
                (String.concat "/" output) in
            let expected_output_file =
              BuildSubst.subst s expected_output_file in
            let expected_output =
              try File.string_of_file expected_output_file
              with _ -> failwith "Missing expected output"
            in
            let output_len = String.length cmd_output in
            let expected_len = String.length expected_output in
            if output_len < expected_len then
              failwith
                (Printf.sprintf "output too short (%d < %d)"
                   output_len  expected_len);
            if output_len > expected_len then
              failwith
                (Printf.sprintf "output too long (%d > %d)"
                   output_len  expected_len);
            let rec iter pos =
              if pos < String.length cmd_output then begin
                if cmd_output.[pos] <> expected_output.[pos] then
                  failwith (Printf.sprintf "output differ at char %d" pos);
                iter (pos+1)
              end
            in
            iter 0;
            stats.tests_nsuccesses <- stats.tests_nsuccesses + 1
          with Failure s ->
            stats.tests_nfailures <- stats.tests_nfailures + 1;
            stats.tests_failures <- (lib, test, s)
              :: stats.tests_failures
          end
        end
        else begin
          stats.tests_nfailures <- stats.tests_nfailures + 1;
          stats.tests_failures <- (lib, test,
            Printf.sprintf "status=%d" status) :: stats.tests_failures
        end
      ) tests
    end;
    Unix.chdir cwd;
  with e ->
    Unix.chdir cwd;
    raise e

let finish tests =
  let ntotal = tests.tests_nsuccesses + tests.tests_nfailures in
  Printf.printf "SUCCESS: %d/%d\n" tests.tests_nsuccesses ntotal;
  Printf.printf "FAILED: %d/%d\n" tests.tests_nfailures ntotal;
  List.iter (fun (lib,s, result) ->
    Printf.printf "  %s.%s (%s)\n%!" lib.lib_name s result
  ) tests.tests_failures;
  if tests.tests_nfailures > 0 then exit 1
