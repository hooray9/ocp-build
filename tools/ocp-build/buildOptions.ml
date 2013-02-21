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

(* Options are stored in:
   - in the project directory, in a file called 'ocp-build.root' that is
   also used as to discover the root of the project.
   - in the user HOME directory, in a file called '~/.ocp/ocp-build.conf'

   As a consequence, we have two sets of options:
   - project options, that are usually optional options
   - global options, that have the default value

   Finally, we must handle also arguments, that can be used to change the
   final value of the option.

*)

(*
TODO: we might need a mechanism at some point to declare options in
other files. For now, it is better to declare all options here.
*)

open SimpleConfig

(* 3 configuration files:
 - one for the user preferences: $OCP_HOME/ocp-build.prefs
 - one for OCaml configuration: $OCPBUILD_CONF/ocp-build.conf
 - one for the project: ocp-build.root
*)

type config_input = {
  mutable cin_ocamlc_variants : string list;
  mutable cin_ocamlopt_variants : string list;
  mutable cin_ocamldep_variants : string list;
  mutable cin_ocamllex_variants : string list;
  mutable cin_ocamlyacc_variants : string list;
  mutable cin_bytecode : bool;
  mutable cin_native : bool;
  mutable cin_ocamlbin : string option;
  mutable cin_ocamllib : string option;
  mutable cin_use_ocamlfind : bool;
  mutable cin_ocps_in_ocamllib : bool;
  mutable cin_meta_dirnames : string list;
  mutable cin_ocps_dirnames : string list;

  mutable cin_autoscan : bool;
  mutable cin_digest : bool;
  mutable cin_verbosity : int;
  mutable cin_njobs : int;

  mutable cin_install_bin : string option;
  mutable cin_install_lib : string option;
  mutable cin_install_doc : string option;
  mutable cin_install_data : string option;
}

(********    Misc functions     *********)

type arg_action =
  | LoadFile of SimpleConfig.config_file * string
  | SaveFile of SimpleConfig.config_file * string
  | SetInt of int SimpleConfig.config_option * int
  | SetBool of bool SimpleConfig.config_option * bool
  | SetBoolOption of bool option SimpleConfig.config_option * bool
  | SetFalse of bool SimpleConfig.config_option
  | SetString of string SimpleConfig.config_option * string
  | SetStrings of string list SimpleConfig.config_option * string
  | SetStringOption of string option SimpleConfig.config_option * string
  | SetStringsOption of string list option SimpleConfig.config_option * string

let save_configs = ref []
let arguments = ref []

let argumentize s =
  let s = String.copy s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '_' -> s.[i] <- '-'
    | _ -> ()
  done;
  s

let arg_set_int int_option =
  ("-" ^ argumentize (LowLevel.shortname int_option),
    Arg.Int (fun n ->
      arguments := (SetInt (int_option, n)) :: !arguments;
    ), " " ^LowLevel.get_help int_option)

let arg_set_true bool_option =
  ("-" ^ argumentize (LowLevel.shortname bool_option),
    Arg.Unit (fun () ->
      arguments := (SetBool (bool_option, true)) :: !arguments;
    ), " " ^LowLevel.get_help bool_option)

let arg_set_true2 bool_option bool_option2 =
  ("-" ^ argumentize (LowLevel.shortname bool_option),
    Arg.Unit (fun () ->
      arguments := (SetBool (bool_option, true)) :: !arguments;
      arguments := (SetBoolOption (bool_option2, true)) :: !arguments;
    ), " " ^LowLevel.get_help bool_option)

let arg_set_false bool_option =
  ("-no-" ^ argumentize (LowLevel.shortname bool_option) ,
    Arg.Unit (fun () ->
      arguments := (SetFalse bool_option) :: !arguments;
    ), " Unset option. " ^ LowLevel.get_help bool_option)

let arg_set_false2 bool_option bool_option2 =
  ("-no-" ^ argumentize (LowLevel.shortname bool_option),
    Arg.Unit (fun () ->
      arguments := (SetBool (bool_option, false)) :: !arguments;
      arguments := (SetBoolOption (bool_option2, false)) :: !arguments;
    ), " Unset option. " ^ LowLevel.get_help bool_option)

let arg_set_strings option =
  ("-" ^ argumentize (LowLevel.shortname option),
    Arg.String (fun s ->
      arguments := (SetStrings (option,s)) :: !arguments;
    ), "STRING " ^ LowLevel.get_help option)

let arg_set_string_option option =
  ("-" ^ argumentize (LowLevel.shortname option),
    Arg.String (fun s ->
      arguments := (SetStringOption (option,s)) :: !arguments;
    ), "STRING " ^ LowLevel.get_help option)

let load_config config filename =
  if not (File.X.exists filename) then begin
    try
      Printf.eprintf
        "Warning: file %S does not exist. Creating with default values.\n%!"
        (File.to_string filename);
      File.Dir.make_all (File.dirname filename);

      SimpleConfig.set_config_file config filename;
      SimpleConfig.save_with_help config
    with e ->
      Printf.eprintf "Warning: could not save file %S\n%!"
        (File.to_string filename)
  end else
    try
      SimpleConfig.append config filename
    with e ->
      Printf.eprintf
        "Warning: Exception %S while loading config file %S\n%!"
        (Printexc.to_string e) (File.to_string filename)


let apply_arguments () =
  List.iter (fun arg_action ->
    match arg_action with
    | LoadFile (config, filename) ->
      load_config config (File.of_string filename)
    | SaveFile (config, filename) ->
      save_configs := (config, filename) :: !save_configs
    | SetInt (option,n) -> option =:= n
    | SetBool (option, t) -> option =:= t
    | SetBoolOption (option, t) -> option =:= Some t
    | SetFalse option -> option =:= false
    | SetString (option, s) -> option =:= s
    | SetStrings (option, s) -> option =:= [s]
    | SetStringOption (option, s) -> option =:= Some s
    | SetStringsOption (option, s) -> option =:= Some [s]
  ) (List.rev !arguments);

  List.iter (fun (config, filename) ->
    SimpleConfig.set_config_file config (File.of_string filename);
    SimpleConfig.save_with_help config) !save_configs

(********    Startup code     *********)

let user_home_dir = try Sys.getenv "HOME" with _ -> "."
let user_config_dir =
  try Sys.getenv "OCP_HOME" with _ ->
    Filename.concat user_home_dir ".ocp"
let user_config_filename = Filename.concat user_config_dir "ocp-build.prefs"

let ocaml_config_dir =
  try Sys.getenv "OCPBUILD_CONF" with _ -> user_config_dir
let ocaml_config_filename =
  Filename.concat ocaml_config_dir "ocp-build.conf"

let project_config_basename = "ocp-build.root"



(********    User preferences     *********)

module UserOptions = struct

  let must_save_config = ref false
  let user_config_file = File.of_string user_config_filename
  let user_config = SimpleConfig.create_config_file user_config_file

  let njobs_option =
    SimpleConfig.create_option user_config
      [ "njobs" ]
      ["Maximal number of jobs started in parallel (0 = auto-detect)" ]
      int_option 0

  let verbosity_option =
    SimpleConfig.create_option user_config
      [ "verbosity" ] ["Verbosity for debugging";
                       "0 = almost no output,";
                       "1 = shortened output,";
                       "2 = commands, etc." ]
      int_option 1

  let autoscan_option =
    SimpleConfig.create_option user_config
      [ "autoscan" ] ["If set, always scan for new .ocp files" ]
      bool_option true

  let digest_option =
    SimpleConfig.create_option user_config
      [ "digest" ]
      ["If set, use content digest change to trigger recompilation";
       "If not set, use timestamps + inode numbers" ]
      bool_option false

  let bytecode_option =
    SimpleConfig.create_option user_config
      [ "bytecode" ] ["If set, compile in bytecode" ]
      bool_option true

  let native_option =
    SimpleConfig.create_option user_config
      [ "native" ] ["If set, compile in native code" ]
      bool_option true

  let load () =
    load_config user_config user_config_file

  let maybe_save () =
    if !must_save_config then begin
      must_save_config := false;
      try SimpleConfig.save_with_help user_config
      with e ->
        Printf.eprintf "Warning: exception %S while saving user config\n%!"
          (Printexc.to_string e)
    end

end

(********    OCaml Configuration    *********)

module OCamlOptions = struct

  let must_save_config = ref false
  let ocaml_config_file = File.of_string ocaml_config_filename
  let ocaml_config = SimpleConfig.create_config_file ocaml_config_file

  let ocamlbin_option =
    SimpleConfig.create_option ocaml_config
      [ "ocamlbin" ]  ["directory containing ocaml compilers"]
      (option_option string_option) None

  let ocamllib_option =
    SimpleConfig.create_option ocaml_config
      [ "ocamllib" ]  ["directory containing ocaml libraries"]
      (option_option string_option) None

  let ocamlc_option =
    SimpleConfig.create_option ocaml_config
      [ "ocamlc_variants" ]  ["executable names to use in preference order"]
      (list_option string_option) [ "ocamlc.opt"; "ocamlc" ]

  let ocamlopt_option =
    SimpleConfig.create_option ocaml_config
      [ "ocamlopt_variants" ]  ["executable names to use in preference order"]
      (list_option string_option) [ "ocamlopt.opt"; "ocamlopt" ]

  let ocamldep_option =
    SimpleConfig.create_option ocaml_config
      [ "ocamldep_variants" ]  ["executable names to use in preference order"]
      (list_option string_option) [ "ocamldep.opt"; "ocamldep" ]

  let ocamllex_option =
    SimpleConfig.create_option ocaml_config
      [ "ocamllex_variants" ]  ["executable names to use in preference order"]
      (list_option string_option) [ "ocamllex.opt"; "ocamllex" ]

  let ocamlyacc_option =
    SimpleConfig.create_option ocaml_config
      [ "ocamlyacc_variants" ]
      ["executable names to use in preference order"]
      (list_option string_option) [ "ocamlyacc" ]

  let meta_dirnames_option =
    SimpleConfig.create_option ocaml_config
      [ "meta_dirnames" ]
      ["List of directories where to\n  look for META files" ]
      (list_option string_option) []

  let ocps_in_ocamllib_option =
    SimpleConfig.create_option ocaml_config
      [ "ocps_in_ocamllib" ] ["If set, load .ocp files from\n  OCAMLLIB" ]
      bool_option true

  let ocps_dirnames_option =
    SimpleConfig.create_option ocaml_config
      [ "ocps_dirnames" ]
      ["List of directories where to\n  look for .ocp files" ]
      (list_option string_option) []



  let load () =
    try
      if File.X.exists ocaml_config_file then
        SimpleConfig.load ocaml_config
    with e ->
      Printf.eprintf
        "Warning: Exception %S while loading OCaml config file %S\n%!"
        (Printexc.to_string e) (File.to_string ocaml_config_file)


  let maybe_save () =
    if !must_save_config then begin
      must_save_config := false;
      try SimpleConfig.save_with_help ocaml_config
      with e ->
        Printf.eprintf "Warning: exception %S while saving OCaml config\n%!"
          (Printexc.to_string e)
    end

end

(********    Project Configuration    *********)

module ProjectOptions = struct

  let must_save_config = ref false
  let project_config = SimpleConfig.create_config_file (File.of_string "")

  (*** First, lets override user's preferences *****)

  let digest_option =
    SimpleConfig.create_option project_config
      [ "digest" ]
      ["If set, use content digest change to trigger recompilation";
       "If not set, use timestamps + inode numbers" ]
      (option_option bool_option) None

  let verbosity_option =
    SimpleConfig.create_option project_config
      [ "verbosity" ] ["Verbosity for debugging";
                       "0 = almost no output,";
                       "1 = shortened output,";
                       "2 = commands, etc." ]
      (option_option int_option) None

  let njobs_option =
    SimpleConfig.create_option project_config
      [ "njobs" ]
      ["Maximal number of jobs started in parallel (0 = auto-detect)" ]
      (option_option int_option) None

  let autoscan_option =
    SimpleConfig.create_option project_config
      [ "autoscan" ] ["If set, always scan for new .ocp files" ]
      (option_option bool_option) None

  let bytecode_option =
    SimpleConfig.create_option project_config
      [ "bytecode" ] ["If set, compile in bytecode" ]
      (option_option bool_option) None

  let native_option =
    SimpleConfig.create_option project_config
      [ "native" ] ["If set, compile in native code" ]
      (option_option bool_option) None

  let install_bin_option =
    SimpleConfig.create_option project_config
      [ "install_bin" ]  ["where programs should be installed"]
      (option_option string_option) None

  let install_lib_option =
    SimpleConfig.create_option project_config
      [ "install_lib" ]  ["where libraries should be installed"]
      (option_option string_option) None

  let install_data_option =
    SimpleConfig.create_option project_config
      [ "install_data" ]  ["where multi-arch data should be installed"]
      (option_option string_option) None

  let install_doc_option =
    SimpleConfig.create_option project_config
      [ "install_doc" ]  ["where documentation files should be installed"]
      (option_option string_option) None

  let ocamllib_option =
    SimpleConfig.create_option project_config
      [ "ocamllib" ]  ["directory containing ocaml libraries"]
      (option_option string_option) None

  let use_ocamlfind_option =
    SimpleConfig.create_option project_config
      [ "use_ocamlfind" ] ["If set, use ocamlfind to locate\n  META files" ]
      bool_option true

  let project_ocpbuild_version = create_option project_config
      [ "ocpbuild_version" ]
      ["The version of ocp-build used to save this file"]
      SimpleConfig.string_option BuildVersion.version

  let project_external_dirs_option = create_option project_config
    [ "project_external_dirs" ]
    [ "All external (absolute) directories to be included in this project" ]
    (SimpleConfig.list_option SimpleConfig.string_option) []

  let root_files = create_option project_config [ "files" ]
   [ "List of configuration files for this project"]
    (list_option file_option) []


  let load project_config_dir =
    let project_config_file = File.add_basename project_config_dir
        project_config_basename in
    try
      if File.X.exists project_config_file then begin
        SimpleConfig.set_config_file project_config project_config_file;
        SimpleConfig.load project_config
      end
    with e ->
      Printf.eprintf
        "Warning: Exception %S while loading Project config file %S\n%!"
        (Printexc.to_string e) (File.to_string project_config_file)


  let maybe_save () =
    if !must_save_config then begin
      must_save_config := false;
      try SimpleConfig.save_with_help project_config
      with e ->
        Printf.eprintf "Warning: exception %S while saving project config\n%!"
          (Printexc.to_string e)
    end

end

let njobs_arg = ref None
let verbosity_arg = ref None
let asm_arg = ref None
let byte_arg = ref None
let scan_arg = ref None
let digest_arg = ref None
let use_ocamlfind_arg = ref None
let install_bin_arg = ref None
let install_lib_arg = ref None
let install_doc_arg = ref None
let install_data_arg = ref None

let arg_list () =
  [

(* user preferences *)
(* ocaml options *)
    "-save-user-prefs", Arg.Unit (fun () ->
      UserOptions.must_save_config := true
    ),
    " Save user preferences";

(* ocaml options *)
    "-ocaml-config", Arg.String (fun s ->
      arguments := (LoadFile (OCamlOptions.ocaml_config, s)) :: !arguments),
    "FILENAME Load ocaml config from FILENAME";
    "-save-ocaml-config", Arg.String (fun s ->
      if s <> "-" then
        arguments := (SaveFile (OCamlOptions.ocaml_config, s)) :: !arguments
      else
        OCamlOptions.must_save_config := true
    ),
    "FILENAME Save ocaml config to FILENAME (- for default file)";

(**** asm option ****)

    "-asm", Arg.Unit (fun () ->
      asm_arg := Some true), " Compile in native code";
    "-no-asm", Arg.Unit (fun () ->
      asm_arg := Some true), " Do not compile in native code";
    "-project-asm", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.native_option =:= Some true),
    " Compile native code version of the project (project option)";
    "-project-no-asm", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.native_option =:= Some false),
    " Do not compile native code version of the project (project option)";

(**** byte option ****)

    "-byte", Arg.Unit (fun () ->
      byte_arg := Some true), " Compile in bytecode";
    "-no-byte", Arg.Unit (fun () ->
      byte_arg := Some true), " Do not compile in bytecode";
    "-project-byte", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.bytecode_option =:= Some true),
    " Compile bytecode code version of the project (project option)";
    "-project-no-byte", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.bytecode_option =:= Some false),
    " Do not compile bytecode code version of the project (project option)";

(**** autoscan option ****)

    "-scan", Arg.Unit (fun () ->
      scan_arg := Some true), " Scan sub-directories";
    "-no-scan", Arg.Unit (fun () ->
      scan_arg := Some false), " Don't scan sub-directories";
    "-project-autoscan", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.autoscan_option =:= Some true),
    " Always scan project for new package (project option)";
    "-project-no-autoscan", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.autoscan_option =:= Some false),
    " Do not scan directory by default (project option)";

(**** digest option ****)

    "-digest", Arg.Unit (fun () ->
      digest_arg := Some true),
    " Use check-sums";
    "-no-digest", Arg.Unit (fun () ->
      digest_arg := Some false),
    " Do not use checksums";
    "-project-digest", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.digest_option =:= Some true),
    " Use checksums (project option)";
    "-project-no-digest", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.digest_option =:= Some false),
    " Do not use checksums (project option)";

(**** njobs option ****)

    "-njobs", Arg.Int (fun n ->
      njobs_arg := Some n),
    "NUM Number of processes to start in parallel";
    "-project-njobs", Arg.Int (fun n ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.njobs_option =:= Some n),
    "NUM Number of processes to start in parallel (project option)";

(**** verbosity option ****)

    "-verbosity", Arg.Int (fun n ->
      verbosity_arg := Some n),
    "NUM Verbosity level";
    "-project-verbosity", Arg.Int (fun n ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.verbosity_option =:= Some n),
    "NUM Verbosity level (project option)";

(**** use-ocamlfind option ****)

    "-use-ocamlfind", Arg.Unit (fun () ->
      use_ocamlfind_arg := Some true),
    " Try to Use ocamlfind to locate META files";
    "-no-use-ocamlfind", Arg.Unit (fun () ->
      use_ocamlfind_arg := Some false),
    " Don't try to use ocamlfind to locate META files";
    "-project-use-ocamlfind", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.use_ocamlfind_option =:= true),
    " Try to Use ocamlfind to locate META files (project option)";
    "-project-no-use-ocamlfind", Arg.Unit (fun () ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.use_ocamlfind_option =:= false),
    " Don't try to use ocamlfind to locate META files (project option)";


(**** install-bin option ****)

    "-install-bin", Arg.String (fun s ->
      install_bin_arg := Some s),
    "FILENAME Directory where binaries should be installed";
    "-project-install-bin", Arg.String (fun s ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.install_bin_option =:= Some s),
    "FILENAME Directory where binaries should be installed (project option)";

(**** install-lib option ****)

    "-install-lib", Arg.String (fun s ->
      install_lib_arg := Some s),
    "FILENAME Directory where libraries should be installed";
    "-project-install-lib", Arg.String (fun s ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.install_lib_option =:= Some s),
    "FILENAME Directory where libraries should be installed (project option)";

(**** install-doc option ****)

    "-install-doc", Arg.String (fun s ->
      install_doc_arg := Some s),
    "FILENAME Directory where documentation should be installed";
    "-project-install-doc", Arg.String (fun s ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.install_doc_option =:= Some s),
    "FILENAME Directory where documentation should be installed (project option)";

(**** install-data option ****)

    "-install-data", Arg.String (fun s ->
      install_data_arg := Some s),
    "FILENAME Directory where data files should be installed";
    "-project-install-data", Arg.String (fun s ->
      ProjectOptions.must_save_config := true;
      ProjectOptions.install_data_option =:= Some s),
    "FILENAME Directory where data files should be installed (project option)";

    arg_set_true            OCamlOptions.ocps_in_ocamllib_option;
    arg_set_false           OCamlOptions.ocps_in_ocamllib_option;
    arg_set_strings         OCamlOptions.ocamlc_option;
    arg_set_strings         OCamlOptions.ocamlc_option;
    arg_set_strings         OCamlOptions.ocamlopt_option;
    arg_set_strings         OCamlOptions.ocamldep_option;
    arg_set_strings         OCamlOptions.ocamllex_option;
    arg_set_strings         OCamlOptions.ocamlyacc_option;
    arg_set_string_option   OCamlOptions.ocamlbin_option;
    arg_set_string_option   OCamlOptions.ocamllib_option;


  ]


let must_save_project () =
  ProjectOptions.must_save_config := true


let some_or_default project_option user_option =
  match !!project_option  with
    None -> !! user_option
  | Some bool -> bool

let load project_dir =

  UserOptions.load ();
  OCamlOptions.load ();
  begin
    match project_dir with
      None -> ()
    | Some project_dir ->
      ProjectOptions.load project_dir
  end;

  apply_arguments ();

  let cin_autoscan =
    match !scan_arg with
    None ->
      some_or_default ProjectOptions.autoscan_option UserOptions.autoscan_option
    | Some arg -> arg
  in
  let cin_native =
    match !scan_arg with
    None ->
      some_or_default ProjectOptions.native_option UserOptions.native_option
    | Some arg -> arg
  in
  let cin_bytecode =
    match !scan_arg with
    None ->
      some_or_default ProjectOptions.bytecode_option UserOptions.bytecode_option
    | Some arg -> arg
  in
  let cin_digest =
    match !digest_arg with
    None ->
      some_or_default ProjectOptions.digest_option UserOptions.digest_option
    | Some arg -> arg
  in
  let cin_verbosity =
    match !verbosity_arg with
    None ->
      some_or_default ProjectOptions.verbosity_option
        UserOptions.verbosity_option
    | Some verb -> verb
  in
  let cin_njobs =
    match !njobs_arg with
    None ->
      some_or_default ProjectOptions.njobs_option UserOptions.njobs_option
    | Some njobs -> njobs
  in

  let cin_use_ocamlfind =
    match !use_ocamlfind_arg with
      None -> !!ProjectOptions.use_ocamlfind_option
    | Some arg -> arg
  in

  let cin_install_bin = match !install_bin_arg with
      None -> !!ProjectOptions.install_bin_option
    | arg -> arg
  in

  let cin_install_lib = match !install_lib_arg with
      None -> !!ProjectOptions.install_lib_option
    | arg -> arg
  in

  let cin_install_doc = match !install_doc_arg with
      None -> !!ProjectOptions.install_doc_option
    | arg -> arg
  in

  let cin_install_data = match !install_data_arg with
      None -> !!ProjectOptions.install_data_option
    | arg -> arg
  in

  let cin = {
    cin_bytecode;
    cin_native;
    cin_autoscan;

    cin_njobs;
    cin_digest;
    cin_verbosity;

    cin_use_ocamlfind;

    cin_ocamlc_variants = !!OCamlOptions.ocamlc_option;
    cin_ocamlopt_variants = !!OCamlOptions.ocamlopt_option;
    cin_ocamldep_variants = !!OCamlOptions.ocamldep_option;
    cin_ocamlyacc_variants = !!OCamlOptions.ocamlyacc_option;
    cin_ocamllex_variants = !!OCamlOptions.ocamllex_option;
    cin_ocamlbin = !!OCamlOptions.ocamlbin_option;
    cin_ocamllib = !!OCamlOptions.ocamllib_option;
    cin_ocps_in_ocamllib = !!OCamlOptions.ocps_in_ocamllib_option;
    cin_meta_dirnames = !!OCamlOptions.meta_dirnames_option;
    cin_ocps_dirnames = !!OCamlOptions.ocps_dirnames_option;
    cin_install_bin;
    cin_install_lib;
    cin_install_doc;
    cin_install_data;

  }
  in
  cin

let maybe_save () =
  UserOptions.maybe_save ();
  OCamlOptions.maybe_save ();
  ProjectOptions.maybe_save ()




let rec shortcut_arg new_name old_name list =
  match list with
      [] ->
    Printf.eprintf "Could not find old option %S for new option %S\n%!"
      old_name new_name;
    assert false
    | (name, f, help) :: list ->
      if name = old_name then
        (new_name, f,
         Printf.sprintf "%s\n    (shortcut for %s)" help old_name)
      else shortcut_arg new_name old_name list
