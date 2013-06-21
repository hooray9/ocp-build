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

open BuildOCamlConfig.TYPES
open BuildTerm
open BuildOptions
(* open BuildBase *)
open BuildGlobals
open BuildOCPTypes
open BuildTypes
open BuildEngineTypes
open BuildEngineGlobals

let time_steps = ref []
let time_step (msg : string) =
  time_steps := (msg , Unix.gettimeofday()) :: !time_steps

let verbose = DebugVerbosity.verbose [ "B" ] "BuildActions"

let deleted_files = ref []

let delete_file filename =
  deleted_files := filename :: !deleted_files;
  if not !fake_arg then
    Unix.unlink filename

let rec delete_file_or_directory filename =
  begin try
	  let files = Sys.readdir filename in
	  Array.iter (fun file ->
	    delete_file_or_directory (Filename.concat filename file)) files
    with _ -> ()
  end;
  begin try
	  delete_file filename
    with _ -> try
		Unix.rmdir filename
      with _ -> ()
  end

let init_deleted () = deleted_files := []

let print_deleted () =
  if verbose 2 then begin
    Printf.eprintf "Deleted files%s:"
      (if !fake_arg then "(faked)" else "");
    List.iter (fun filename ->
      Printf.eprintf " %s" filename
    ) !deleted_files;
    Printf.eprintf "\nTotal: %d files\n%!" (List.length !deleted_files);
    deleted_files := [];
  end


(*
  init_deleted ();
  Hashtbl.iter (fun _ file ->
    match file.file_kind with
	FILE_VIRTUAL -> ()
      | FILE_REAL | FILE_TEMPORARY ->
	    let filename = file_filename file in
	    match file.file_target_of with
	[] -> ()
      |  _ ->

	let _, ext = File.cut_last_extension file.file_basename in
(*	Printf.eprintf "Testing generated file %s[%s]\n" filename ext; *)

	match ext with
	    "o" | "a" | "cmi" | "cmo" | "cmx" | "cmxa"
	  | "cma" | "byte" | "opt" | "mlpp" | "mlipp" ->

	    if Sys.file_exists filename then begin
(*	      Printf.eprintf "\tscheduling for deletion\n"; *)
	      delete_file filename
	    end else
	      Printf.eprintf "\tfile does not exist\n";
	  | _ -> ()
  ) b.build_files;
  print_deleted ()


let clean_map =
  StringMap.of_list (List.map (fun x ->
  x, delete_file) [
  (* Generated files *)
  "o"; "cmo"; "cmi"; "cmx"; "cma"; "cmxa"; "annot"; "a";
    "byte"; "asm"; "opt";
    "mldep"; "mlidep";
   (* Modified source files *)
  "ml~"; "mli~"; "mll~"; "mly~"; "ocp~";
  ])

let do_distclean () =
  init_deleted ();
  delete_file_or_directory "_obuild";
  let map = ref clean_map in
  List.iter (fun (x,y) ->
    map := StringMap.add x y !map
  ) [
    ".mll", (fun filename ->
      let kernel_name = Filename.chop_suffix filename ".mll" in
      delete_file (kernel_name ^ ".ml"));
    ".mly", (fun filename ->
      let kernel_name = Filename.chop_suffix filename ".mly" in
      delete_file (kernel_name ^ ".ml");
      delete_file (kernel_name ^ ".mli");
    );
  ];
  BuildScanner.scan_directory_for_extensions "." !map;
  print_deleted ();
  ()
*)

let list_of_ocp_files_filename = "ocp-build.root"

type project_info = {
  project_dir : File.t;
  cin : BuildOptions.config_input;
  install_where : BuildOCamlInstall.install_where;
  cout : BuildOCamlConfig.TYPES.config_output;
  cfg : BuildOCamlConfig.TYPES.ocaml_config;
}

let load_project () =
  let project_dir = BuildOptions.find_project_root () in

  time_step "Loading configuration files...";
  let cin =  BuildOptions.load project_dir in
  time_step "   Done loading files";

  DebugVerbosity.increase_verbosity "B" cin.cin_verbosity;
  BuildTerm.set_ansi_term (term.esc_ansi && cin.cin_color);

  time_step "Checking OCaml config...";
  let cout = BuildOCamlConfig.check_config cin in

  let cfg = match cout.cout_ocaml with
      None -> assert false (* TODO : for now *)
    | Some cfg -> cfg
  in
  time_step "   Done checking OCaml config.";


  let install_where =
    let open BuildOCamlInstall in

    {
      install_destdir = cin.cin_install_destdir;
      install_libdirs = (match cin.cin_install_lib with
          None ->
          begin match cout.cout_meta_dirnames with
            [] -> [cfg.ocaml_ocamllib]
            | _ -> cout.cout_meta_dirnames
          end
        | Some dir -> [dir]);
      install_bindir = (match cin.cin_install_bin with
          None -> cfg.ocaml_bin
        | Some dir -> dir);
      install_datadir = cin.cin_install_data;

      install_ocamllib = cfg.ocaml_ocamllib;
      install_ocamlfind = cout.cout_meta_dirnames;
    }
  in
  {
    project_dir;
    cin;
    install_where;
    cout;
    cfg;
  }
