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

(* TODO: we should save the version of ocaml used to build a project,
   so that we can detect changes and ask for a clean before building.
   Can we access the magic used by every compiler ? (we can compile an
   empty file in bytecode and native code) We could cache this
   information using the uniq identifier of the executable (would not
   work with wrappers).
*)

(* TODO
  We could force packages with missing dependencies to still be compiaboutled,
  since it is still possible that these missing dependencies are not used
  in a particular compilation scheme.
*)

open OcpLang
open SimpleConfig

open BuildConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions

let _ = DebugVerbosity.add_submodules "B" [ "BuildMain" ]

let initial_verbosity =
  try
    Some (int_of_string (Sys.getenv "OCPBUILD_VERBOSITY"))
  with _ -> None

let version = BuildVersion.version

let print_version () =
  Printf.fprintf stderr "%s\n%!" version;
  exit 0

let set_verbosity v =
  DebugVerbosity.increase_verbosity "B" v

let t0 = Unix.gettimeofday ()

let time s f x =
  if !time_arg then
    let t0 = Unix.gettimeofday () in
    let y = f x in
    let t1 = Unix.gettimeofday () in
    Printf.printf s (t1 -. t0);
    y
  else
    f x

let time_initial = Unix.gettimeofday ()
let timer_find_project = ref 0.
let timer_load_config = ref 0.
let timer_create_context = ref 0.
let timer_configure = ref 0.
let timer_save_local = ref 0.
let timer_find_env = ref 0.
let timer_load_env = ref 0.
let timer_load_meta = ref 0.
let timer_load_project = ref 0.
let timer_sort_packages = ref 0.
let timer_create_rules = ref 0.
let timer_init_build = ref 0.
let timer_check_sanitize = ref 0.
let timer_build_project = ref 0.

let timer0 = ref time_initial

let add_timing msg timer f x =
  let t0 = Unix.gettimeofday () in
(*  Printf.eprintf "discarded before %s: %.2f\n%!" msg (t0 -. !timer0); *)
  timer0 := t0;
  try
    let y = f x in
    let timer1 = Unix.gettimeofday () in
    timer := (timer1 -. !timer0) +. !timer;
    timer0 := timer1;
    y
  with e ->
    let timer1 = Unix.gettimeofday () in
    timer := (timer1 -. !timer0) +. !timer;
    timer0 := timer1;
    raise e

let save_project = ref false
let save_arguments_arg = ref false
let delete_orphans_arg = ref KeepOrphans
let list_projects_arg = ref false
let list_byte_targets_arg = ref false
let list_asm_targets_arg = ref false
let build_dir_basename_arg = ref "_obuild"
let no_stdlib_arg = ref None
let other_dirs_arg = ref []
type arch_arg = ArchAuto | ArchNone | Arch of string
let arch_arg = ref ArchNone

let install_arg = ref false
let uninstall_arg = ref false
let install_lib_arg = ref None
let install_data_arg = ref None
let install_bin_arg = ref None
let meta_dirnames_arg = ref []
let meta_verbose_arg = ref 0
let list_installed_arg = ref false
let install_bundle_arg = ref None

let define_args = ref []

let init_arg = ref false

let arg_list = BuildOptions.arg_list ()

let arg_list = [
  "-init", Arg.Unit (fun () ->
    init_arg := true;
    save_arguments_arg := true;
    save_project := true), " : create the ocp-build.root file";

  "-version", Arg.Unit (fun () ->
    Printf.printf "%s\n%!" BuildVersion.version;
    exit 0
  ), "Print version information";

  "-about", Arg.Unit (fun () ->
    Printf.printf "ocp-build : OCaml Project Builder\n";
    Printf.printf "\tversion: %s\n" BuildVersion.version;
    Printf.printf "\tdescription: %s\n" BuildVersion.description;
    List.iter (fun author ->
      Printf.printf "\tauthor: %s\n" author) BuildVersion.authors;
    Printf.printf "\tlicense: %s\n" BuildVersion.license;
    Printf.printf "%!";
    exit 0
  ), "Print version information";

  "-define", Arg.String (fun s ->
    define_args := s :: !define_args;
    let (_  : bool BuildOCPVariable.source_option)
        = BuildOCPVariable.new_initial_bool_option s true in

    ()
  ),
  " OPTION : define an initial option";

  "-installed", Arg.Set list_installed_arg,
  "List installed packages";

  "-meta-verbose", Arg.Int (fun n -> meta_verbose_arg := n),
  "Set verbosity of loading META files";

  "-meta", Arg.String (fun s -> meta_dirnames_arg := s :: !meta_dirnames_arg),
  "Load META files from this directory";

  "-install", Arg.Set install_arg,
  "Install binaries and libraries";

  "-uninstall", Arg.Set uninstall_arg,
  "Uninstall given packages (installed by ocp-build)";

  "-install-bundle", Arg.String (fun s -> install_bundle_arg := Some s),
  "Install a bundle packages to uninstall all packages at once";

  "-install-lib", Arg.String (fun s ->
    if Filename.is_relative s then begin
      Printf.eprintf "Error: argument to -install-lib must be absolute\n%!";
      exit 2
    end;
    install_lib_arg := Some s),
  "Specify directory where libraries should be installed";

  "-install-bin", Arg.String (fun s ->
    if Filename.is_relative s then begin
      Printf.eprintf "Error: argument to -install-bin must be absolute\n%!";
      exit 2
    end;
    install_bin_arg := Some s),
  "Specify directory where binaries should be installed";

  "-install-data", Arg.String (fun s ->
    if Filename.is_relative s then begin
      Printf.eprintf "Error: argument to -install-data must be absolute\n%!";
      exit 2
    end;
    install_data_arg := Some s),
  "Specify directory where data should be installed (if any)";

(*
  "-byte", Arg.Set byte_arg, " : build only bytecode version";
  "-asm", Arg.Set asm_arg, " : build only native code version";
*)
  "-clean", Arg.Set clean_arg, " : clean all compiled files and exit";
  "-distclean", Arg.Set distclean_arg, " : clean all generated files and exit";

  "-obuild", Arg.String (fun s -> build_dir_basename_arg := s), " <dir> : change _obuild directory";
  "-arch", Arg.String (fun s -> arch_arg := Arch s), " <arch> : set arch";
  "-auto-arch", Arg.Unit (fun () -> arch_arg := ArchAuto), ": set arch automatically";
  "-I", Arg.String (fun s -> other_dirs_arg := s :: !other_dirs_arg),
  " DIRECTORY : add DIRECTORY to project";
  "-no-stdlib", Arg.Unit (fun _ -> no_stdlib_arg := Some true), " : do not scan standard directory";
  "-sanitize", Arg.Unit (fun _ -> delete_orphans_arg := DeleteOrphanFiles), " : remove orphan objects from _obuild";
  "-sanitize-dirs", Arg.Unit (fun _ -> delete_orphans_arg := DeleteOrphanFilesAndDirectories), " : remove orphan directories from _obuild";

  "-list-ocp-files", Arg.Set list_ocp_files, " : list all .ocp files found";
  "-cross", Arg.String (fun arch -> cross_arg := Some arch), " : use a cross-compilation directory";
  "-k", Arg.Clear stop_on_error_arg, " : continue after errors";
  "-fake", Arg.Set fake_arg, " : fake actions, do not execute them";

  "-use-pp", Arg.Set use_pp, " : force use of -pp";

  "-list-projects", Arg.Set list_projects_arg, " : list projects";
  "-list-targets", Arg.Unit (fun _ ->
    list_byte_targets_arg := true;
    list_asm_targets_arg := true), " : list all targets";
  "-list-byte-targets", Arg.Set list_byte_targets_arg, " : list bytecode targets";
  "-list-asm-targets", Arg.Set list_asm_targets_arg, " : list native targets";

  "-time", Arg.Set time_arg, " : print timing";

  "-library-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name LibraryPackage
      (File.of_string "."); exit 0;
  ), " <name> : auto-generate a .ocp file for a library";

  "-program-ocp", Arg.String (fun name ->
    BuildAutogen.create_package name ProgramPackage
      (File.of_string "."); exit 0;
  ), " <name> : auto-generate a .ocp file for a library";

]
  @ [
    "", Arg.Unit (fun _ -> ()),
    String.concat "\n" [
      " : useless argument";
      "";
      "Options under this line will be saved if you either use";
      "\"-save-global\" or \"-save-local\":";
      ""
    ];
    BuildOptions.shortcut_arg "-asm" "-native" arg_list;
    BuildOptions.shortcut_arg "-byte" "-bytecode" arg_list;
    BuildOptions.shortcut_arg "-scan" "-autoscan" arg_list;
    BuildOptions.shortcut_arg "-v" "-verbosity" arg_list;
  ]
  @ arg_list

let arg_usage =
  String.concat "\n"
    ["ocp-build [options] targets : build OCaml targets";
     "";
     "A project is composed of packages, described by .ocp files";
     "";
     "The following options are available:"
    ]

let build () =
  targets_arg := List.rev !targets_arg;


  let project_basenames = [  "ocp-build.root" ] in
  let project_dir =
    try
      BuildOCP.find_root (File.X.getcwd ()) project_basenames
    with Not_found ->

            (* time "Config time: %.2fs\n%!" BuildConfig.load_config
               local_config_file; *)

      let pjo = {
        option_njobs = 0;
        option_autoscan = false;
        option_verbosity = 0;
        option_usestdlib = false;
        option_digest = false;
        option_native = true;
        option_bytecode = true;
        option_ocamlbin = "";
        option_ocamllib = "";
        option_ocamlc = [];
        option_ocamlopt = [];
        option_ocamldep = [];
        option_ocamllex = [];
        option_ocamlyacc = [];
        option_installbin = ""; (* TODO not used ??? *)
        option_installlib = None; (* TODO not used ??? *)
      } in

      let cfg = BuildConfig.check_config pjo in
      let ocamlfind_path = MetaConfig.load_config () in

      let install_where =
              let open BuildOCamlInstall in

              {
                install_libdirs = (match !install_lib_arg with
                  None -> ocamlfind_path @ [cfg.ocaml_ocamllib]
                | Some dir -> [dir]);
                install_bindir = (match !install_bin_arg with
                  None -> cfg.ocaml_bin
                | Some dir -> dir);
                install_datadir = !install_data_arg;

                install_ocamllib = cfg.ocaml_ocamllib;
                install_ocamlfind = ocamlfind_path;
              }
            in

            if !list_installed_arg then begin

              let open BuildOCamlInstall in
              Printf.printf "Installed packages:\n";
              List.iter (fun un ->
                Printf.printf "\t%s . %s (%s)\n%!" un.un_name un.un_version un.un_type
              ) (BuildOCamlInstall.list_installed install_where);
              exit 0

            end else

              if !uninstall_arg && !targets_arg <> [] then begin

                List.iter (BuildOCamlInstall.uninstall_by_name install_where)
                  !targets_arg;
                exit 0
              end
              else
                begin

                  Printf.fprintf stderr "Fatal error: no ocp-build.root file found.\n%!";
                  Printf.fprintf stderr "\tYou can use the -init option at the root of the project\n";
                  Printf.fprintf stderr "\tto create the initial file.\n%!";
                  exit 2
                end
        in

        Unix.chdir (File.to_string project_dir);
        Printf.fprintf stdout "ocp-build: Entering directory `%s'\n%!"  (File.to_string project_dir);
        try

          let root_file = File.add_basenames project_dir project_basenames in
          let root_dir = File.dirname root_file in

          let build_dir_basename = !build_dir_basename_arg in


          let build_dir_filename = (* absolute_filename *) build_dir_basename in

          if verbose 3 then Printf.eprintf "Arguments parsed\n%!";


          let root_config, pjo =
            add_timing "load ocp-build.conf" timer_load_config
            BuildOptions.load_local root_file in

          if pjo.option_ocamllib <> "" then begin
    (* do this before check_config *)
            Unix.putenv "OCAMLLIB" pjo.option_ocamllib
          end;

  (*  time "Config time: %.2fs\n%!" BuildConfig.load_config local_config_file; *)
          let cfg =
            add_timing
              "check config"
              timer_configure BuildConfig.check_config pjo in

          let ocamlfind_path =
            add_timing  "check ocamlfind"
              timer_configure MetaConfig.load_config () in

          let install_where =
      let open BuildOCamlInstall in

      {
        install_libdirs = (match !install_lib_arg with
          None -> ocamlfind_path @ [cfg.ocaml_ocamllib]
        | Some dir -> [dir]);
        install_bindir = (match !install_bin_arg with
          None -> cfg.ocaml_bin
        | Some dir -> dir);
        install_datadir = !install_data_arg;

        install_ocamllib = cfg.ocaml_ocamllib;
        install_ocamlfind = ocamlfind_path;
      }
    in

    if !list_installed_arg then begin

      let open BuildOCamlInstall in
      Printf.printf "Installed packages:\n";
      List.iter (fun un ->
        Printf.printf "\t%s . %s (%s)\n%!" un.un_name un.un_version un.un_type
      ) (BuildOCamlInstall.list_installed install_where);
      exit 0

    end else

      if !uninstall_arg && !targets_arg <> [] then

        List.iter (BuildOCamlInstall.uninstall_by_name install_where)
          !targets_arg

      else

  let host = Printf.sprintf "%s-%s-%s"
    cfg.ocaml_system cfg.ocaml_architecture cfg.ocaml_version in

  let build_dir_filename =
    match !arch_arg with
      ArchAuto -> Filename.concat build_dir_filename host
    | Arch host -> Filename.concat build_dir_filename host
    | ArchNone -> build_dir_filename
  in
(*  Printf.fprintf stderr "build_dir_filename = %s\n%!" build_dir_filename; *)
  let b =
    add_timing "create context" timer_create_context
    (BuildEngineContext.create (File.to_string root_dir))
    build_dir_filename in


  b.stop_on_error_arg <- !stop_on_error_arg;

  if !conf_arg || !distrib_arg || !autogen_arg then exit 0;


  let project_ocpbuild_version = create_option root_config
    [ "ocpbuild_version" ]
    ["The version of ocp-build used to save this file"]
    SimpleConfig.string_option version
  in

  if !!project_ocpbuild_version != BuildVersion.version then
    project_ocpbuild_version =:= BuildVersion.version;

  let _project_other_dirs_option = create_option root_config
    [ "other_dirs" ]
    [ "Other directories to be scanned" ]
    (SimpleConfig.list_option SimpleConfig.string_option) []
  in

  let force_scan = pjo.option_autoscan in
  let use_digests = pjo.option_digest in

  set_verbosity pjo.option_verbosity;

  let ncores = pjo.option_njobs in

  let ncores =
    if ncores < 1 then
      add_timing "find n cores" timer_configure
        BuildConfig.number_of_cores () + 1
    else
      ncores
  in

  let _usestdlib = pjo.option_usestdlib in

  let root_files = create_option root_config [ "files" ]
   [ "List of configuration files for this project"]
    (list_option file_option) []
  in

  if use_digests then BuildEngineMtime.use_digests true;
  if force_scan then begin
    save_project := true;

    let files = add_timing "find project"
      timer_find_project
      BuildOCP.scan_root
      root_dir in
    root_files =:= files (* !!project_other_dirs *)
  end;

  if !save_project then begin
    Printf.fprintf stderr "Updating ocp-build.root\n%!";
    BuildOptions.must_save_local true
  end;
  add_timing "save local" timer_save_local
    BuildOptions.maybe_save_local root_config;

    b.cross_arg <- !cross_arg;


  (* Don't modify default values from now on, since they have been included
   in the default configuration ! *)

    let env_dirs = ref [
      cfg.ocaml_ocamllib;
    ] in
    let env_files = ref [] in
    List.iter (fun dir ->
      Printf.printf "Scanning installed .ocp files in %S\n%!" dir;
      let dir = File.of_string dir in
      env_files := (add_timing "find env" timer_find_env
                      BuildOCP.scan_root dir) @ !env_files)
      !env_dirs;

    if !!root_files = [] then begin
      Printf.eprintf "Error: no known .ocp files\n";
      Printf.eprintf "\tHave you run ocp-build with -scan to find them ?\n%!";
      exit 2
    end;

    let state = BuildOCP.init_packages () in

    let _nerrors1 =
      let config = BuildOCP.generated_config () in
      add_timing "load env" timer_load_env
      (BuildOCP.load_ocp_files config state)  !env_files
    in

    add_timing "load meta" timer_load_meta
    List.iter (fun dirname ->
      let dirs =
        if dirname = "" || dirname = "-" then
        match ocamlfind_path with
          [] -> [cfg.ocaml_ocamllib]
        | list -> list
        else
          [dirname]
      in
      List.iter (BuildOCamlMeta.load_META_files state cfg) dirs
    ) !meta_dirnames_arg;

   let nerrors =
     let config = BuildOCP.empty_config () in
      add_timing "load project" timer_load_project
        (BuildOCP.load_ocp_files config state) !!root_files
    in

    if nerrors > 0 then exit 2;
    let pj =
      add_timing "sort packages" timer_sort_packages
      BuildOCP.verify_packages state in

    BuildOCP.save_project_state pj
      (File.add_basenames root_dir ["_obuild"; "ocp.ocpx"]);

  let print_package pj = Printf.eprintf "\t%s in %s (%s,%s)\n"
    pj.package_name pj.package_dirname
	(BuildOCPTree.string_of_package_type pj.package_type)
        pj.package_source_kind
      in

    if verbose 5 || !list_projects_arg then begin

      Printf.eprintf "Disabled packages:\n";
      Array.iter print_package pj.project_disabled;

      Printf.eprintf "Validated packages:\n";
      Array.iter print_package pj.project_sorted;
    end;

  begin
    let incomplete_projects = Hashtbl.create  13 in
    if pj.project_incomplete <> [||] then begin
      Printf.eprintf "Warning: %d incomplete projects:\n" (Array.length pj.project_incomplete);
      let meta_need = ref 0 in
      Array.iter (fun pk ->
        Hashtbl.add incomplete_projects pk.package_name pk;
        if !meta_verbose_arg > 0 ||
          pk.package_source_kind <> "meta" then (* TODO ? *)
          print_package pk
        else
          incr meta_need
      )
        pj.project_incomplete;
      if !meta_need > 0 then
        Printf.eprintf "Also %d incomplete packages in META files not printed.\n%!" !meta_need
    end;

  List.iter (fun (name, list) ->
    let non_meta_need = ref false in
    if !meta_verbose_arg > 0 then
      non_meta_need := true
    else
      List.iter (fun pk ->
        if pk.package_source_kind <> "meta" then non_meta_need := true
      ) list;
    if !non_meta_need then begin
      Printf.eprintf "   %s \"%s\" missed by %d projects\n"
        (if Hashtbl.mem incomplete_projects name then "INCOMPLETE" else "ABSENT")
        name
        (List.length list);
      List.iter print_package list;
    end;
  ) pj.project_missing;
  end;

  add_timing "create rules" timer_create_rules
    (BuildOCamlRules.create pj) b;

  if !distclean_arg then begin
    BuildActions.do_distclean ();
    exit 0
  end;

  if !clean_arg then begin
    BuildActions.do_clean b;
    exit 0;
  end;

  if !list_byte_targets_arg then begin
    Printf.eprintf "Bytecode targets:\n";
    StringMap.iter (fun _ lib ->
      if lib.lib_byte_targets <> [] then begin
	List.iter (fun (target, kind) ->
          Printf.eprintf "\t%s\t->\t%s\n" lib.lib_name target.file_basename)
          lib.lib_byte_targets;
      end) !packages_by_name;
	Printf.eprintf "%!"
  end;

  if !list_asm_targets_arg then begin
    Printf.eprintf "Native targets:\n";
    StringMap.iter (fun _ lib ->
      if lib.lib_asm_targets <> [] then begin
	List.iter (fun (target, kind) ->
          Printf.eprintf "\t%s\t->\t%s\n" lib.lib_name target.file_basename)
          lib.lib_asm_targets;
      end) !packages_by_name;
	Printf.eprintf "%!"
  end;

(* build the list of projects considered by the current command *)
  let projects = ref [] in
  begin
    match !targets_arg with
	[] ->
	  StringMap.iter (fun _ pj ->
            projects := pj :: !projects) !packages_by_name
      | list ->
	List.iter (fun name ->
	  try
	    let pj = StringMap.find name !packages_by_name in
	    projects := pj :: !projects
	  with Not_found ->
	    Printf.eprintf "Error: Could not find target project %s\n%!" name;
	    exit 2
	) list
  end;

  add_timing "B" timer_load_config (fun _ -> ()) ();

  if !uninstall_arg then
    List.iter (fun lib ->
      BuildOCamlInstall.uninstall install_where lib)
      !projects
  else
    let install_what =

      let open BuildOCamlInstall in
      {
        install_asm_bin = true;
        install_byte_bin = true;
        install_asm_lib = true;
        install_byte_lib = true;
      }
    in

    if !install_arg then

      let already_installed = ref 0 in

      let bundle = match !install_bundle_arg with
          None -> None
        | Some name ->
          if BuildOCamlInstall.is_installed
            install_where
            name then begin
              Printf.eprintf "Error: bundle %S is already installed\n%!" name;
              exit 2
            end;
          match BuildOCamlInstall.find_installdir
            install_where install_what
            name with
              None -> exit 2
            | Some installdir ->
              Some (name, installdir)
      in

      List.iter (fun pj ->
        if not pj.lib_installed &&
          BuildOCPVariable.bool_option_with_default pj.lib_options
          "install" true &&
          BuildOCamlInstall.is_installed
          install_where
          pj.lib_name then begin
            Printf.eprintf "Error: %S is already installed\n%!" pj.lib_name;
            incr already_installed
          end
      ) !projects;
      if !already_installed > 0 then begin
        Printf.eprintf "Error: %d packages are already installed. Uninstall them first !\n%!" !already_installed;
        exit 2
      end;


      let uninstallers = ref [] in
      List.iter (fun pj ->
        if not pj.lib_installed &&
          BuildOCPVariable.bool_option_with_default pj.lib_options
          "install" true
        then
          match       BuildOCamlInstall.find_installdir
            install_where install_what
            pj.lib_name with
              None -> ()
            | Some installdir ->
              BuildOCamlInstall.install
                install_where install_what
                pj installdir;
              uninstallers := (Filename.concat installdir
                                 (pj.lib_name ^ ".uninstall")) :: !uninstallers
      )
        !projects;

      begin match bundle with
        None -> ()
      | Some (name, installdir) ->
        BuildOCamlInstall.install_bundle
          install_where name !uninstallers installdir
      end


    else

      (* build the list of targets *)
      let targets = ref [] in
      let add_project_targets lib =
        if not lib.lib_installed then begin
          if pjo.option_bytecode then
            targets := List.map fst lib.lib_byte_targets @ !targets;
          if pjo.option_native then
            targets := List.map fst lib.lib_asm_targets @ !targets;
        end
      in
      List.iter add_project_targets !projects;

      if !targets = [] then begin
        Printf.eprintf "Error: project contains no targets\n%!";
        Printf.eprintf "\tAre your .ocp files empty ?\n%!";
        exit 2
      end;

      (*
        List.iter (fun s ->
        Printf.eprintf "TARGET %S\n%!" (File.to_string s.file_file)
        ) !targets;
      *)

      if !targets <> [] then begin
        begin
          try
	    add_timing "init build" timer_init_build
              (BuildEngine.init b) !targets
          with BuildEngine.MissingSourceWithNoBuildingRule (r, filename) ->
	    let (rule_filename, rule_loc, rule_name) = r.rule_loc in
	    BuildMisc.print_loc rule_filename rule_loc;
	    Printf.eprintf "Error: in project \"%s\", the source filename\n"
	      rule_name;
	    Printf.eprintf "\t\"%s\" does not exist\n" filename;
	    BuildEngineRules.print_rule r;
	    exit 2
        end;
        let orphans = add_timing "check sanitize" timer_check_sanitize
          (BuildEngine.sanitize b) !delete_orphans_arg in
        if orphans > 0 then begin
          Printf.fprintf stderr "Error: found %d orphan files in _obuild. You must remove them.\n" orphans;
          Printf.fprintf stderr "\n";
          Printf.fprintf stderr "   You can add the -sanitize argument to automatically remove\n";
          Printf.fprintf stderr "   orphan files\n";
          Printf.fprintf stderr "\n";
          exit 2;
        end else
          if orphans < 0 then
            Printf.fprintf stderr "Warning: deleted %d orphan files in _obuild\n" (-orphans);
        Printf.fprintf stderr "Building using %d cores\n%!" ncores;
        add_timing "build project"
          timer_build_project (BuildEngine.parallel_loop b) ncores;
        let errors = BuildEngine.fatal_errors() @ BuildEngineDisplay.errors() in
        if verbose 2 || errors <> [] then begin
          Printf.eprintf "%s. %d commands executed, %d files generated.\n%!"
	    (if errors = [] then "No error" else
	        Printf.sprintf "%d errors" (List.length errors))
	    !BuildEngine.stats_command_executed
	    !BuildEngine.stats_files_generated;
        end;
        if errors <> [] then begin
          List.iter (fun lines ->
	    Printf.eprintf "Error:\n";
	    List.iter (fun line ->
	      Printf.eprintf "%s\n" line
	    ) lines

          ) errors;
          exit 2
        end;
      end;
      Printf.eprintf "%!";
      let t1 = Unix.gettimeofday () in
      Printf.printf "Successfully built in %.2fs\n%!" (t1 -. t0);
      if !time_arg then begin
        List.iter (fun (msg, timer) ->
          if !timer > 0.009 then
            Printf.printf "\t%4.2fs to %s\n%!" !timer msg
        ) [
          "load ocp-build.conf",timer_load_config;
        "check tools", timer_configure;
        "init VFS", timer_create_context;
        "find project .ocp files", timer_find_project;
        "save ocp-build.root", timer_save_local;
        "find env .ocp files", timer_find_env;
        "load env .ocp files", timer_load_env;
        "load env META files", timer_load_meta;
        "load project .ocp files", timer_load_project;
        "sort packages", timer_sort_packages;
        "instantiate rules", timer_create_rules;
        "init engine", timer_init_build;
        "check sanitize", timer_check_sanitize;
        "build project", timer_build_project;
        ]
      end;

      Printf.fprintf stdout "ocp-build: Leaving directory `%s'\n%!" (File.to_string project_dir)

        with e ->
          let backtrace = Printexc.get_backtrace () in
          Printf.fprintf stdout "ocp-build: Leaving directory `%s'\n%!" (File.to_string project_dir);
          Printf.fprintf stderr "ocp-build: Fatal Exception %s\n%s\n%!" (Printexc.to_string e) backtrace;
          raise e

let _ =
  Printexc.record_backtrace true;

  BuildOptions.load_global ();
  begin match initial_verbosity with None -> () | Some v -> set_verbosity v end;
  Arg.parse arg_list (fun s -> targets_arg := s :: !targets_arg) arg_usage;
  BuildOptions.maybe_save_global ();

  if !init_arg && not (Sys.file_exists "ocp-build.root") then begin
    let oc = open_out "ocp-build.root" in
    close_out oc
  end;
  build ()
