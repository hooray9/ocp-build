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


(* open BuildBase *)
let verbose = DebugVerbosity.verbose [ "B" ] "BuildOCamlConfig"

(* open Stdlib2 *)
open BuildMisc
open BuildOCPVariable
open SimpleConfig
open BuildConfig
open BuildOptions

module TYPES = struct

  type ocaml_config = {
    ocaml_version : string;
    ocaml_version_major : string;
    ocaml_version_minor : string;
    ocaml_version_point : string;
    ocaml_ocamllib : string;
    ocaml_system : string;
    ocaml_architecture : string;
    ocaml_ext_obj : string;
    ocaml_ext_lib : string;
    ocaml_ext_dll : string;
    ocaml_os_type : string;
    ocaml_bin : string;
  }

  type config_output = {
    mutable cout_ocaml : ocaml_config option;
    mutable cout_ocamlc : string list option;
    mutable cout_ocamlcc : string list option;
    mutable cout_ocamlopt : string list option;
    mutable cout_ocamldep : string list option;
    mutable cout_ocamlyacc : string list option;
    mutable cout_ocamllex : string list option;
    mutable cout_meta_dirnames : string list;
  }

end

open TYPES


let get_config cmd =
  let _, c = get_stdout_lines [ cmd ] [ "-config" ] in
  let ocaml_version = ref "NOVERSION" in
  let ocaml_system = ref "NOSYSTEM" in
  let ocaml_architecture = ref "NOARCH" in
  let ocaml_ext_obj = ref ".o" in
  let ocaml_ext_lib = ref ".a" in
  let ocaml_ext_dll = ref ".so" in
  let ocaml_os_type = ref "NOOSTYPE" in
  let ocaml_ocamllib = ref "" in
  List.iter (fun line ->
    let (name, v) = OcpString.cut_at line ':' in
    let v = String.sub v 1 (String.length v - 1) in
    match name with
      | "version" -> ocaml_version := v
      | "system" -> ocaml_system := v
      | "architecture" -> ocaml_architecture := v
      | "ext_obj" -> ocaml_ext_obj := v
      | "ext_lib" -> ocaml_ext_lib := v
      | "ext_dll" -> ocaml_ext_dll := v
      | "os_type" -> ocaml_os_type := v
      | "standard_library" -> ocaml_ocamllib := v
      | _ -> ()
  ) c;
  let (major, minor, point) = split_version !ocaml_version in

  { ocaml_version = !ocaml_version;
    ocaml_version_major = major;
    ocaml_version_minor = minor;
    ocaml_version_point = point;
    ocaml_architecture = !ocaml_architecture;
    ocaml_system = !ocaml_system;
    ocaml_ext_obj = !ocaml_ext_obj;
    ocaml_ext_lib = !ocaml_ext_lib;
    ocaml_ext_dll = !ocaml_ext_dll;
    ocaml_os_type = !ocaml_os_type;
    ocaml_ocamllib = !ocaml_ocamllib;
    ocaml_bin = Filename.dirname cmd;
  }

let check_is_compiler ocamlc_prefixes args ocamlc =
  let status, lines =
    try get_stdout_lines [ ocamlc ] args
    with e ->
      Printf.eprintf "Warning: could not execute %S\n%!" ocamlc;
      Printf.eprintf "\texception %S\n%!" (Printexc.to_string e);
      (-1, []) in
  if status = 0 then
    try
      match lines with
          first_line :: _ ->
            let prefix =
              let pos = String.index first_line ',' in
              String.sub first_line 0 pos
            in
            List.mem prefix ocamlc_prefixes
        | _ -> false
 with _ -> false
 else false

let ocamlc_prefixes = [
  "The Objective Caml compiler"; "The OCaml compiler"]
let ocamlopt_prefixes = [
  "The Objective Caml native-code compiler";
  "The OCaml native-code compiler"
]
let ocamldep_prefixes = [ "ocamldep" ]
let ocamllex_prefixes = [
  "The Objective Caml lexer generator";
  "The OCaml lexer generator" ]
let ocamlyacc_prefixes = [
  "The Objective Caml parser generator";
  "The OCaml parser generator" ]

let check_is_ocamlc = check_is_compiler ocamlc_prefixes  [ "-v" ]
let check_is_ocamlopt = check_is_compiler ocamlopt_prefixes  [ "-v" ]
let check_is_ocamllex = check_is_compiler ocamllex_prefixes  [ "-version" ]
let check_is_ocamldep = check_is_compiler ocamldep_prefixes [ "-version" ]
let check_is_ocamlyacc = check_is_compiler ocamlyacc_prefixes [ "-version" ]


let check_config cin =

  let cout = {
    cout_ocamlc = None;
    cout_ocamlcc = None;
    cout_ocamlopt = None;
    cout_ocamllex = None;
    cout_ocamlyacc = None;
    cout_ocamldep = None;
    cout_ocaml = None;
    cout_meta_dirnames = [];
  } in

  if not (cin.cin_native || cin.cin_bytecode) then begin
    cin.cin_native <- true;
    cin.cin_bytecode <- true;
  end;

  if cin.cin_ocamlc_variants = [] then
    cin.cin_ocamlc_variants <- [ "ocamlc.opt" ; "ocamlc"];
  if cin.cin_ocamlopt_variants = [] then
    cin.cin_ocamlopt_variants <- [ "ocamlopt.opt" ; "ocamlopt"];
  if cin.cin_ocamldep_variants = [] then
    cin.cin_ocamldep_variants <- [ "ocamldep.opt" ; "ocamldep"];
  if cin.cin_ocamllex_variants = [] then
    cin.cin_ocamllex_variants <- [ "ocamllex.opt" ; "ocamllex"];
  if cin.cin_ocamlyacc_variants = [] then
    cin.cin_ocamlyacc_variants <- [ "ocamlyacc"];

  let path =
    match cin.cin_ocamlbin with
      None -> get_PATH ()
    | Some ocamlbin ->  [ ocamlbin ]
  in

  begin match cin.cin_ocamllib with None -> () | Some ocamllib ->
    Unix.putenv "OCAMLLIB" ocamllib
  end;


  let ocamlc = find_first_in_path path check_is_ocamlc
      cin.cin_ocamlc_variants in
  let ocamlopt = find_first_in_path path check_is_ocamlopt
      cin.cin_ocamlopt_variants in

  (* TODO: for now, we fail if we can't find a valid OCaml compiler. Maybe later,
     we could be ok if we are compiling OCaml... *)
  let cfg = match ocamlc, ocamlopt with
    | None, None ->
      Printf.eprintf "Error: could not find an OCaml compiler.\n";
      exit 2

    | Some ocamlc, None ->
      if cin.cin_native then begin
        if cin.cin_bytecode then begin
          Printf.eprintf "Warning: could not find an OCaml native code compiler\n";
          Printf.eprintf "\tDeactivating native code generation.\n";
        end
        else begin
          Printf.eprintf "Error: could not find an OCaml native code compiler\n";
          Printf.eprintf "\tbut you ask for native code only.\n";
          exit 2
        end;
      end;
      cin.cin_native <- false;
      cout.cout_ocamlc <- Some [ocamlc];
      cout.cout_ocamlcc <- Some [ocamlc];
      get_config ocamlc

    | None, Some ocamlopt ->
      if cin.cin_bytecode then begin
        if cin.cin_native then begin
          Printf.eprintf "Warning: could not find an OCaml bytecode compiler\n";
          Printf.eprintf "\tDesactivating bytecode generation.\n";
          exit 2
        end
        else begin
          Printf.eprintf "Error: could not find an OCaml bytecode compiler\n";
          Printf.eprintf "\tbut you ask for bytecode only.\n";
          exit 2
        end;
      end;
      cin.cin_bytecode <- false;
      cout.cout_ocamlopt <- Some [ocamlopt];
      cout.cout_ocamlcc <- Some [ocamlopt];
      get_config ocamlopt

    | Some ocamlc, Some ocamlopt ->
      let byte_config = get_config ocamlc in
      let native_config = get_config ocamlopt in
      if byte_config <> native_config then begin
        Printf.fprintf stderr "Warning: bytecode and native code compilers disagree on configuration%!\n";
        if cin.cin_bytecode && cin.cin_native then begin
          Printf.eprintf "\tDesactivating bytecode generation.\n";
          cin.cin_native <- false;
        end
      end;
      cout.cout_ocamlc <- Some [ocamlc];
      cout.cout_ocamlopt <- Some [ocamlopt];
      cout.cout_ocamlcc <- Some [ocamlc];
      byte_config
  in

  cout.cout_ocaml <- Some cfg;

  let ocamldep = find_first_in_path path
      check_is_ocamldep cin.cin_ocamldep_variants in
  begin match ocamldep with
      None ->
      Printf.eprintf "Error: Could not find OCaml ocamldep tool.\n";
      exit 2
    | Some ocamldep ->
      cout.cout_ocamldep <- Some [ocamldep]
  end;

  let ocamllex = find_first_in_path path
      check_is_ocamllex cin.cin_ocamllex_variants in
  begin match ocamllex with
      None ->
      (* TODO: this should only be an error if a .mll file has to be compiled *)
      Printf.eprintf "Error: Could not find OCaml ocamllex tool.\n";
      exit 2
    | Some ocamllex ->
      cout.cout_ocamllex <- Some [ocamllex];
  end;

  let ocamlyacc = find_first_in_path path
      check_is_ocamlyacc cin.cin_ocamlyacc_variants in
  begin match ocamlyacc with
      None ->
      Printf.eprintf "Error: Could not find OCaml ocamlyacc tool.\n";
      exit 2
    | Some ocamlyacc ->
      cout.cout_ocamlyacc <- Some [ocamlyacc];
  end;

  let ocamlfind_path =
    if cin.cin_use_ocamlfind then
      MetaConfig.load_config ()
    else [] in

  let meta_dirnames =
    ocamlfind_path @
    cin.cin_meta_dirnames in

  let rec filter_dirnames path =
    match path with
      [] | "END" :: _ -> []
    | "OCAML" :: tail -> cfg.ocaml_ocamllib :: (filter_dirnames tail)
    | dir :: tail -> dir :: (filter_dirnames tail)
  in
  let meta_dirnames = filter_dirnames meta_dirnames in
  cout.cout_meta_dirnames <- meta_dirnames;



  cout


let ocamldep_cmd = new_strings_option "ocamldep" [ "ocamldep.opt" ]
let ocamlc_cmd = new_strings_option "ocamlc" [ "ocamlc.opt" ]
let ocamlcc_cmd = new_strings_option "ocamlcc" [ "ocamlc.opt" ]
let ocamlopt_cmd = new_strings_option "ocamlopt" [ "ocamlopt.opt" ]
let ocamllex_cmd = new_strings_option "ocamllex" [ "ocamllex.opt" ]
let ocamlyacc_cmd = new_strings_option "ocamlyacc" [ "ocamlyacc" ]


let ocaml_config_version = new_strings_option "ocaml_version" []
let ocaml_major_version = new_strings_option "ocaml_major_version" [ ]
let ocaml_minor_version = new_strings_option "ocaml_minor_version" [ "00" ]
let ocaml_point_version = new_strings_option "ocaml_point_version" [ "0" ]
let ocaml_config_system = new_strings_option "system" []
let ocaml_config_architecture = new_strings_option "architecture" []
let ocaml_config_ext_obj = new_string_option "ext_obj"  ".o"
let ocaml_config_ext_lib = new_string_option "ext_lib"  ".a"
let ocaml_config_ext_dll = new_string_option "ext_dll"  ".so"
let ocaml_config_os_type = new_strings_option "os_type" [ ]

let set_global_config cout =
  (match cout.cout_ocamlc with None -> () | Some cmd ->
    ocamlc_cmd.set cmd);
  (match cout.cout_ocamlopt with None -> () | Some cmd ->
    ocamlopt_cmd.set cmd);
  (match cout.cout_ocamlcc with None -> () | Some cmd ->
    ocamlcc_cmd.set cmd);
  (match cout.cout_ocamldep with None -> () | Some cmd ->
    ocamldep_cmd.set cmd);
  (match cout.cout_ocamllex with None -> () | Some cmd ->
    ocamllex_cmd.set cmd);
  (match cout.cout_ocamlyacc with None -> () | Some cmd ->
    ocamlyacc_cmd.set cmd);

  let cfg = match cout.cout_ocaml with
      None -> assert false (* TODO : for now *)
    | Some cfg -> cfg
  in

  ocaml_config_ext_lib.set cfg.ocaml_ext_lib;
  ocaml_config_ext_obj.set cfg.ocaml_ext_obj;
  ocaml_config_version.set [cfg.ocaml_version];
  ocaml_major_version.set [ cfg.ocaml_version_major ];
  ocaml_minor_version.set [ cfg.ocaml_version_minor ];
  ocaml_point_version.set [ cfg.ocaml_version_point ];

  ocaml_config_system.set [cfg.ocaml_system];
  ocaml_config_architecture.set [cfg.ocaml_architecture];
  ocaml_config_os_type.set [cfg.ocaml_os_type];
  ocaml_config_ext_dll.set cfg.ocaml_ext_dll;

  BuildSubst.add_to_global_subst "OCAMLLIB" cfg.ocaml_ocamllib;

(*  Printf.fprintf stderr "SYSTEM = %s\n%!" cfg.ocaml_system; *)
()
