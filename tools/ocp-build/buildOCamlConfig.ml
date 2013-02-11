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

let verbose = DebugVerbosity.verbose [ "B" ] "BuildOCamlConfig"

open BuildOCPVariable
open SimpleConfig
open BuildConfig


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

type config_input = {
  mutable cin_ocamlc : string list;
  mutable cin_ocamlopt : string list;
  mutable cin_ocamldep : string list;
  mutable cin_ocamllex : string list;
  mutable cin_ocamlyacc : string list;
  mutable cin_bytecode : bool;
  mutable cin_native : bool;
  mutable cin_ocamlbin : string option;
  mutable cin_ocamllib : string option;
}

type config_output = {
  mutable cout_ocaml : ocaml_config option;
  mutable cout_ocamlc : string list option;
  mutable cout_ocamlcc : string list option;
  mutable cout_ocamlopt : string list option;
  mutable cout_ocamldep : string list option;
  mutable cout_ocamlyacc : string list option;
  mutable cout_ocamllex : string list option;
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

let rec find_first_in_path path filter list =
  match list with
      [] -> None
    | basename :: others ->
      try
        let binary = find_in_PATH basename path in
        if filter binary then Some binary else raise Not_found
      with Not_found ->
        find_first_in_path path filter others

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


let cin_options = SimpleConfig.create_config_file (File.of_string "")


let bytecode_option =
  SimpleConfig.create_option cin_options
  [ "bytecode" ] ["If set, compile in bytecode" ]
  bool_option true
let native_option =
  SimpleConfig.create_option cin_options
  [ "native" ] ["If set, compile in native code" ]
  bool_option true

let ocamlbin_option =
  SimpleConfig.create_option cin_options
  [ "ocamlbin" ]  ["directory containing ocaml compilers"]
  (option_option string_option) None

let ocamllib_option =
  SimpleConfig.create_option cin_options
  [ "ocamllib" ]  ["directory containing ocaml libraries"]
  (option_option string_option) None

let ocamlc_option =
  SimpleConfig.create_option cin_options
  [ "ocamlc" ]  ["executable names to use in preference order"]
  (list_option string_option) [ "ocamlc.opt"; "ocamlc" ]

let ocamlopt_option =
  SimpleConfig.create_option cin_options
  [ "ocamlopt" ]  ["executable names to use in preference order"]
  (list_option string_option) [ "ocamlopt.opt"; "ocamlopt" ]

let ocamldep_option =
  SimpleConfig.create_option cin_options
  [ "ocamldep" ]  ["executable names to use in preference order"]
  (list_option string_option) [ "ocamldep.opt"; "ocamldep" ]

let ocamllex_option =
  SimpleConfig.create_option cin_options
  [ "ocamllex" ]  ["executable names to use in preference order"]
  (list_option string_option) [ "ocamllex.opt"; "ocamllex" ]

let ocamlyacc_option =
  SimpleConfig.create_option cin_options
  [ "ocamlyacc" ]  ["executable names to use in preference order"]
  (list_option string_option) [ "ocamlyacc" ]

let installbin_option =
  SimpleConfig.create_option cin_options
  [ "installbin" ]  ["where programs should be installed"]
  string_option "/usr/local/bin"



type arg_action =
  | LoadFile of string
  | SetTrue of bool SimpleConfig.config_option
  | SetString of string SimpleConfig.config_option * string
  | SetStrings of string list SimpleConfig.config_option * string
  | SetStringOption of string option SimpleConfig.config_option * string
  | SetStringsOption of string list option SimpleConfig.config_option * string

let arg_save_ocaml_config = ref None
let arguments = ref []

let arg_set_true bool_option =
  ("-" ^ LowLevel.shortname bool_option,
    Arg.Unit (fun () ->
      arguments := (SetTrue bool_option) :: !arguments;
    ), "BOOL " ^ LowLevel.get_help bool_option)

let arg_set_strings option =
  ("-" ^ LowLevel.shortname option,
    Arg.String (fun s ->
      arguments := (SetStrings (option,s)) :: !arguments;
    ), "STRING " ^ LowLevel.get_help option)

let arg_set_string_option option =
  ("-" ^ LowLevel.shortname option,
    Arg.String (fun s ->
      arguments := (SetStringOption (option,s)) :: !arguments;
    ), "STRING " ^ LowLevel.get_help option)


let arg_list () =
  [
    "-ocaml-config", Arg.String (fun s ->
      arguments := (LoadFile s) :: !arguments),
    "FILENAME Load ocaml config from FILENAME";
    "-save-ocaml-config", Arg.String (fun s ->
      match !arg_save_ocaml_config with
        Some _ ->
        Printf.eprintf "Error: cannot use twice -save-ocaml-config\n%!";
        exit 2
      | None ->
        arg_save_ocaml_config := Some s),
    "FILENAME Save ocaml config from FILENAME";

    arg_set_true bytecode_option;
    arg_set_true native_option;
    arg_set_strings ocamlc_option;
    arg_set_strings ocamlopt_option;
    arg_set_strings ocamldep_option;
    arg_set_strings ocamllex_option;
    arg_set_strings ocamlyacc_option;
    arg_set_string_option ocamlbin_option;
    arg_set_string_option ocamllib_option;
  ]

let load_config_options filename =
  SimpleConfig.append cin_options filename

let load_global_config filename =
  if not (File.X.exists filename) then begin
    try
      SimpleConfig.set_config_file cin_options filename;
      SimpleConfig.save_with_help cin_options
    with e ->
      Printf.eprintf "Warning: could not save file %S\n%!"
        (File.to_string filename)
  end else
    load_config_options filename

let apply_arguments () =
  List.iter (fun arg_action ->
    match arg_action with
    | LoadFile filename -> load_config_options (File.of_string filename)
    | SetTrue option -> option =:= true
    | SetString (option, s) -> option =:= s
    | SetStrings (option, s) -> option =:= [s]
    | SetStringOption (option, s) -> option =:= Some s
    | SetStringsOption (option, s) -> option =:= Some [s]
  ) (List.rev !arguments);

  match !arg_save_ocaml_config with
  None -> ()
  | Some filename ->
    SimpleConfig.set_config_file cin_options (File.of_string filename);
    SimpleConfig.save_with_help cin_options

let check_config () =

  apply_arguments ();

  let cin = {
    cin_ocamlc = !!ocamlc_option;
    cin_ocamlopt = !!ocamlopt_option;
    cin_ocamldep = !!ocamldep_option;
    cin_ocamlyacc = !!ocamlyacc_option;
    cin_ocamllex = !!ocamllex_option;
    cin_bytecode = !!bytecode_option;
    cin_native = !!native_option;
    cin_ocamlbin = !!ocamlbin_option;
    cin_ocamllib = !!ocamllib_option;
  }
  in

  let cout = {
    cout_ocamlc = None;
    cout_ocamlcc = None;
    cout_ocamlopt = None;
    cout_ocamllex = None;
    cout_ocamlyacc = None;
    cout_ocamldep = None;
    cout_ocaml = None;
  } in

  if not (cin.cin_native || cin.cin_bytecode) then begin
    cin.cin_native <- true;
    cin.cin_bytecode <- true;
  end;

  if cin.cin_ocamlc = [] then
    cin.cin_ocamlc <- [ "ocamlc.opt" ; "ocamlc"];
  if cin.cin_ocamlopt = [] then
    cin.cin_ocamlopt <- [ "ocamlopt.opt" ; "ocamlopt"];
  if cin.cin_ocamldep = [] then
    cin.cin_ocamldep <- [ "ocamldep.opt" ; "ocamldep"];
  if cin.cin_ocamllex = [] then
    cin.cin_ocamllex <- [ "ocamllex.opt" ; "ocamllex"];
  if cin.cin_ocamlyacc = [] then
    cin.cin_ocamlyacc <- [ "ocamlyacc"];

  let path =
    match cin.cin_ocamlbin with
      None -> get_PATH ()
    | Some ocamlbin ->  [ ocamlbin ]
  in

  begin match cin.cin_ocamllib with None -> () | Some ocamllib ->
    Unix.putenv "OCAMLLIB" ocamllib
  end;


  let ocamlc = find_first_in_path path check_is_ocamlc cin.cin_ocamlc in
  let ocamlopt = find_first_in_path path check_is_ocamlopt cin.cin_ocamlopt in

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
      check_is_ocamldep cin.cin_ocamldep in
  begin match ocamldep with
      None ->
      Printf.eprintf "Error: Could not find OCaml ocamldep tool.\n";
      exit 2
    | Some ocamldep ->
      cout.cout_ocamldep <- Some [ocamldep]
  end;

  let ocamllex = find_first_in_path path
      check_is_ocamllex cin.cin_ocamllex in
  begin match ocamllex with
      None ->
      (* TODO: this should only be an error if a .mll file has to be compiled *)
      Printf.eprintf "Error: Could not find OCaml ocamllex tool.\n";
      exit 2
    | Some ocamllex ->
      cout.cout_ocamllex <- Some [ocamllex];
  end;

  let ocamlyacc = find_first_in_path path
      check_is_ocamlyacc cin.cin_ocamlyacc in
  begin match ocamlyacc with
      None ->
      Printf.eprintf "Error: Could not find OCaml ocamlyacc tool.\n";
      exit 2
    | Some ocamlyacc ->
      cout.cout_ocamlyacc <- Some [ocamlyacc];
  end;

  cin, cout


let ocamldep_cmd = new_initial_strings_option "ocamldep" [ "ocamldep.opt" ]
let ocamlc_cmd = new_initial_strings_option "ocamlc" [ "ocamlc.opt" ]
let ocamlcc_cmd = new_initial_strings_option "ocamlcc" [ "ocamlc.opt" ]
let ocamlopt_cmd = new_initial_strings_option "ocamlopt" [ "ocamlopt.opt" ]
let ocamllex_cmd = new_initial_strings_option "ocamllex" [ "ocamllex.opt" ]
let ocamlyacc_cmd = new_initial_strings_option "ocamlyacc" [ "ocamlyacc" ]


let ocaml_config_version = new_initial_strings_option "ocaml_version" []
let ocaml_major_version = new_initial_strings_option "ocaml_major_version" [ ]
let ocaml_minor_version = new_initial_strings_option "ocaml_minor_version" [ "00" ]
let ocaml_point_version = new_initial_strings_option "ocaml_point_version" [ "0" ]
let ocaml_config_system = new_initial_strings_option "system" []
let ocaml_config_architecture = new_initial_strings_option "architecture" []
let ocaml_config_ext_obj = new_initial_strings_option "ext_obj" [ ".o" ]
let ocaml_config_ext_lib = new_initial_strings_option "ext_lib" [ ".a" ]
let ocaml_config_ext_dll = new_initial_strings_option "ext_dll" [ ".so" ]
let ocaml_config_os_type = new_initial_strings_option "os_type" [ ]

let set_global_config cout =
  (match cout.cout_ocamlc with None -> () | Some cmd ->
    set_strings_option ocamlc_cmd cmd);
  (match cout.cout_ocamlopt with None -> () | Some cmd ->
    set_strings_option ocamlopt_cmd cmd);
  (match cout.cout_ocamlcc with None -> () | Some cmd ->
    set_strings_option ocamlcc_cmd cmd);
  (match cout.cout_ocamldep with None -> () | Some cmd ->
    set_strings_option ocamldep_cmd cmd);
  (match cout.cout_ocamllex with None -> () | Some cmd ->
    set_strings_option ocamllex_cmd cmd);
  (match cout.cout_ocamlyacc with None -> () | Some cmd ->
    set_strings_option ocamlyacc_cmd cmd);

  let cfg = match cout.cout_ocaml with
      None -> assert false (* TODO : for now *)
    | Some cfg -> cfg
  in

  set_strings_option ocaml_config_ext_lib [cfg.ocaml_ext_lib];
  set_strings_option ocaml_config_ext_obj [cfg.ocaml_ext_obj];
  set_strings_option ocaml_config_version [cfg.ocaml_version];
  set_strings_option ocaml_major_version [ cfg.ocaml_version_major ];
  set_strings_option ocaml_minor_version [ cfg.ocaml_version_minor ];
  set_strings_option ocaml_point_version [ cfg.ocaml_version_point ];

  set_strings_option ocaml_config_system [cfg.ocaml_system];
  set_strings_option ocaml_config_architecture [cfg.ocaml_architecture];
  set_strings_option ocaml_config_os_type [cfg.ocaml_os_type];
  set_strings_option ocaml_config_ext_dll [cfg.ocaml_ext_dll];

  BuildSubst.add_to_subst "OCAMLLIB" cfg.ocaml_ocamllib;

(*  Printf.fprintf stderr "SYSTEM = %s\n%!" cfg.ocaml_system; *)
()
