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
   since it is still possible that these missing dependencies arbue not used
   in a particular compilation scheme.
*)

open OcpLang
open SimpleConfig

open BuildOCamlConfig.TYPES
open BuildEngineTypes
open BuildOCPTypes
open BuildOCPTree
open BuildTypes
open BuildGlobals
open BuildOptions
open BuildArgs

let _ = DebugVerbosity.add_submodules "B" [ "BuildMain" ]

let print_installed install_where =
  let open BuildOCamlInstall in
  Printf.printf "Installed packages:\n";
  List.iter (fun un ->
    Printf.printf "\t%s . %s (%s)\n%!"
      un.un_name un.un_version un.un_type;
    Printf.printf "\t\tin %s\n%!" un.un_directory;
  ) (BuildOCamlInstall.list_installed install_where);
  ()

let move_to_project = ref true

let finally_do = ref []
let add_finally action =
  finally_do := action :: !finally_do


let do_load_project_files cin project_dir state =
  let open ProjectOptions in

  let force_scan = ref cin.cin_autoscan in

  (* if we didn't find any .ocp files before, we should retry ! *)
  if !!root_files = [] then force_scan := true;

  if ! add_external_projects_arg <> [] then begin
    List.iter (fun dir ->
      if not (List.mem dir !!project_external_dirs_option) then begin
        must_save_project ();
        project_external_dirs_option =:= !!project_external_dirs_option @
            [ dir ];
        force_scan := true;
      end
    ) (List.rev !add_external_projects_arg)
  end;

  if !!project_ocpbuild_version != BuildVersion.version then begin
    must_save_project ();
    project_ocpbuild_version =:= BuildVersion.version;
  end;

  if !force_scan then begin
    save_project := true;
    time_step "Scanning project for .ocp files ...";
    root_files =:= [];
    List.iter (fun dir ->
      let files = BuildOCP.scan_root dir in
      root_files =:= !!root_files @ files
    ) (project_dir ::
        (List.map File.of_string !!project_external_dirs_option));
    time_step "   Done scanning project for .ocp files";
  end;

  if !!root_files = [] then begin
    Printf.eprintf "Error: no known .ocp files\n";
    Printf.eprintf "\tHave you run ocp-build with -scan to find them ?\n%!";
    exit 2
  end;

  time_step "Loading project .ocp files...";
  let nerrors =
    let config = BuildOCP.empty_config () in
    BuildOCP.load_ocp_files config state !!root_files
  in
  time_step "   Done loading project .ocp files";

  if nerrors > 0 then exit 2

let do_install install_where install_what projects =

  let already_installed = ref 0 in

  List.iter (fun pj ->
    if pj.lib_install &&
       BuildOCamlInstall.is_installed
         install_where
         pj.lib_name then begin
      Printf.eprintf "Error: %S is already installed\n%!" pj.lib_name;
      incr already_installed
    end
  ) projects;
  if !already_installed > 0 then begin
    Printf.eprintf
      "Error: %d packages are already installed. Uninstall them first !\n%!"
      !already_installed;
    exit 2
  end;

  let projects_to_install = ref StringMap.empty in
  let rec add_to_install pj =
    if pj.lib_install &&
       not (StringMap.mem pj.lib_name !projects_to_install) then begin
      projects_to_install :=
        StringMap.add pj.lib_name pj !projects_to_install;
      let bundle =
        BuildOCPVariable.list_option_with_default pj.lib_options
          "bundle" [] in
      List.iter (fun name ->
        try
          let pj2 = StringMap.find name !packages_by_name in
          pj2.lib_bundles <- pj :: pj2.lib_bundles
        with Not_found ->
          Printf.eprintf
            "Error: package %S bundled in package %S, not found\n%!"
            pj.lib_name name;
          exit 2
      ) bundle
    end
  in

  List.iter add_to_install projects;
  let install_errors = ref 0 in
  let install_ok = ref 0 in
  StringMap.iter (fun _ pj ->
    if pj.lib_install then
      match       BuildOCamlInstall.find_installdir
          install_where install_what
          pj.lib_name with
        None -> incr install_errors
      | Some installdir ->
        BuildOCamlInstall.install
          install_where install_what
          pj installdir;
        incr install_ok
  )
    !projects_to_install;
  if !install_errors > 0 then begin
    if !install_ok = 0 then
      Printf.eprintf "Install completely failed\n%!"
    else
      Printf.eprintf
        "Install partially failed: %d/%d packages not installed"
        !install_errors (!install_errors + !install_ok);
    exit 2
  end

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
  ) !query_include_dir;

  List.iter (fun p ->
    try
      Array.iter (fun pk ->
        if pk.package_name = p then raise Exit
      ) pj.project_sorted;
      Printf.eprintf "Error: no package %S\n%!" p;
      exit 2
    with Exit ->
      Printf.printf "Package %S is present\n%!" p
  ) !query_has_package_args


let do_print_project_info pj =

  BuildOCP.print_conflicts !print_conflicts_arg;
  let string_of_package pj =
    Printf.sprintf "   %s (%s,%s)\n     in %s\n"
      pj.package_name
      (BuildOCPTree.string_of_package_type pj.package_type)
      pj.package_source_kind
      pj.package_dirname
  in
  let print_package pj =
    Printf.eprintf "%s%!" (string_of_package pj)
  in
  if verbose 5 || !list_projects_arg then begin

    let print_package_array array =
      let list = ref [] in
      Array.iter (fun pj ->
        list := string_of_package pj :: !list) array;
      List.iter (fun s ->
        Printf.printf "%s%!" s)
        (List.sort compare !list)
    in

    Printf.eprintf "Validated packages:\n";
    print_package_array pj.project_sorted;

    Printf.eprintf "Disabled packages:\n";
    print_package_array pj.project_disabled;

  end;

  begin
    let incomplete_packages = Hashtbl.create  13 in
    if pj.project_incomplete <> [||] then begin
      Printf.eprintf "Warning: %d incomplete packages (will not be built):\n"
        (Array.length pj.project_incomplete);
      let meta_need = ref 0 in
      Array.iter (fun pk ->
        Hashtbl.add incomplete_packages pk.package_name pk;
        if !meta_verbose_arg ||
           pk.package_source_kind <> "meta" then (* TODO ? *)
          print_package pk
        else
          incr meta_need
      )
        pj.project_incomplete;
      if !meta_need > 0 then
        Printf.eprintf
          "  Hidden: %d incomplete packages in META files (use -print-incomplete-meta).\n%!" !meta_need
    end;

    if pj.project_missing <> [] then
      let absent_packages = ref [] in
      let other_packages = ref [] in
      List.iter (fun (name, list) ->
        let non_meta_need = ref false in
        if !meta_verbose_arg then
          non_meta_need := true
        else
          List.iter (fun pk ->
            if pk.package_source_kind <> "meta" then non_meta_need := true
          ) list;
        if !non_meta_need then begin
          let packages =
            if Hashtbl.mem incomplete_packages name then
              other_packages else absent_packages in
          packages := (name, list) :: !packages
        end;
      ) pj.project_missing;
      if !absent_packages <> [] then begin
        Printf.eprintf "Warning: %d needed packages are missing !\n%!"
          (List.length !absent_packages);
        List.iter (fun (name, list) ->
          Printf.eprintf "   ABSENT package %S missed by %d packages\n"
            name (List.length list);
          List.iter print_package list;
        ) !absent_packages
      end;
      List.iter (fun (name, list) ->
        Printf.eprintf "   Incomplete package %S missed by %d packages\n"
          name
          (List.length list);
        List.iter print_package list;
      ) !other_packages

  end





let do_init_project_building cfg project_dir pj =
  let build_dir_basename = !build_dir_basename_arg in

  let build_dir_filename = (* absolute_filename *) build_dir_basename in

  let host = Printf.sprintf "%s-%s-%s"
      cfg.ocaml_system cfg.ocaml_architecture cfg.ocaml_version in

  let build_dir_filename =
    match !arch_arg with
      ArchAuto -> Filename.concat build_dir_filename host
    | Arch host -> Filename.concat build_dir_filename host
    | ArchNone -> build_dir_filename
  in

  BuildMisc.safe_mkdir build_dir_filename;

  time_step "Saving raw project info...";
  BuildOCP.save_project_state pj
    (File.add_basename (File.of_string build_dir_filename) "ocp.ocpx");
  time_step "   Done saving raw project info";


  let b =
    BuildEngineContext.create (File.to_string project_dir)
      build_dir_filename in

  b.stop_on_error_arg <- !stop_on_error_arg;

  BuildOCamlRules.create b pj !tests_arg;

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
  b





let do_compile b cin ncores projects =
  (* build the list of targets *)
  let targets = ref [] in
  let map = ref StringMap.empty in
  let rec add_project_targets lib =
    if not lib.lib_installed &&
       (!tests_arg || lib.lib_type <> TestPackage) &&
       not (StringMap.mem lib.lib_name !map) then begin
      if cin.cin_bytecode then
        targets := List.map fst lib.lib_byte_targets @ !targets;
      if cin.cin_native then
        targets := List.map fst lib.lib_asm_targets @ !targets;
      map := StringMap.add lib.lib_name lib !map;
      List.iter (fun dep ->
        if dep.dep_link || dep.dep_syntax then
          add_project_targets dep.dep_project
      ) lib.lib_requires
    end
  in
  List.iter add_project_targets projects;

  if !targets = [] && not !tests_arg then begin
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
    time_step "Initializing build engine...";
    begin

      try
        BuildEngine.init b !targets
      with BuildEngine.MissingSourceWithNoBuildingRule (r, filename) ->
        let (rule_filename, rule_loc, rule_name) = r.rule_loc in
        BuildMisc.print_loc rule_filename rule_loc;
        Printf.eprintf "Error: in project \"%s\", the source filename\n"
          rule_name;
        Printf.eprintf "\t\"%s\" does not exist\n" filename;
        BuildEngineRules.print_rule r;
        exit 2
    end;
    time_step "   Build Engine Initialized";
    time_step "Checking remaining artefacts...";
    let orphans = BuildEngine.sanitize b !delete_orphans_arg
        (fun basename ->
          match basename with
            "_tests" -> true
          | _ -> false)
    in
    if orphans > 0 then begin
      Printf.eprintf "Error: found %d orphan files in %s. You must remove them.\n" orphans !build_dir_basename_arg;
      Printf.eprintf "\n";
      Printf.eprintf "   You can add the -sanitize argument to automatically remove\n";
      Printf.eprintf "   orphan files\n";
      Printf.eprintf "\n";
      exit 2;
    end else
    if orphans < 0 then
      Printf.eprintf
        "Warning: deleted %d orphan files in %s\n" (-orphans) !build_dir_basename_arg;
    time_step "   Done sanitizing";

    time_step "Building packages...";
    let max_nslots = BuildEngine.parallel_loop b ncores
    in
    time_step "   Done building packages";

    let errors = BuildEngine.fatal_errors() @
        BuildEngineDisplay.errors() in
    let t1 = Unix.gettimeofday () in

    let nerrors = List.length errors in
    Printf.eprintf
      "Done in %.2fs: %s. %d jobs (%d parallel), %d files generated.\n%!"
      (t1 -. t0)
      (if errors = [] then "Build Successful" else
         Printf.sprintf "%d error%s" nerrors
           (if nerrors > 1 then "s" else ""))
      !BuildEngine.stats_command_executed
      max_nslots
      !BuildEngine.stats_files_generated;
    if errors <> [] then begin
      Printf.eprintf "Error log:\n";
      List.iter (fun lines ->
        Printf.eprintf "Error:\n";
        List.iter (fun line ->
          Printf.eprintf "%s\n" line
        ) lines
      ) errors;
      exit 2
    end;
  end;
  Printf.eprintf "%!"





let do_test b ncores projects =
  time_step "Executing tests";
  let stats = BuildOCamlTest.init () in
  List.iter (fun lib ->
    match lib.lib_type with
    | ProgramPackage
    | TestPackage ->
      BuildOCamlTest.test_package b stats lib !benchmarks_arg
    | LibraryPackage
    | ObjectsPackage
    | SyntaxPackage
      -> ()
  ) projects;
  BuildOCamlTest.finish stats ncores;
  time_step "   Done executing tests"





let build targets =
  time_step "Arguments parsed.";

  if !query_global then move_to_project := false;

  let project_basenames = [  "ocp-build.root" ] in

  let project_dir =
    if !dont_load_project_arg then None else

      try
        let project_dir =
          BuildOCP.find_root (File.X.getcwd ()) project_basenames in

        Some project_dir
      with Not_found ->
        None
  in

  if !query_root_dir then begin
    match project_dir with
      None ->
      Printf.eprintf "Error: no ocp-build.root\n%!";
      exit 2
    | Some project_dir ->
      Printf.printf "%s\n%!" (File.to_string project_dir);
      exit 0
  end;


  time_step "Loading configuration files...";
  let cin =  BuildOptions.load project_dir in
  time_step "   Done loading files";
  set_verbosity cin.cin_verbosity;

  time_step "Checking OCaml config...";
  let cout = BuildOCamlConfig.check_config cin in

  let cfg = match cout.cout_ocaml with
      None -> assert false (* TODO : for now *)
    | Some cfg -> cfg
  in
  time_step "   Done checking OCaml config.";

  let ocamlfind_path = MetaConfig.load_config () in
  time_step "   Done checking ocamlfind config";

  let install_where =
    let open BuildOCamlInstall in

    {
      install_libdirs = (match cin.cin_install_lib with
          None -> ocamlfind_path @ [cfg.ocaml_ocamllib]
        | Some dir -> [dir]);
      install_bindir = (match cin.cin_install_bin with
          None -> cfg.ocaml_bin
        | Some dir -> dir);
      install_datadir = cin.cin_install_data;

      install_ocamllib = cfg.ocaml_ocamllib;
      install_ocamlfind = ocamlfind_path;
    }
  in

  if !list_installed_arg then begin
    print_installed install_where;
    exit 0
  end;

  let uninstall_state = BuildOCamlInstall.uninstall_init install_where in

  if !uninstall_arg && targets <> [] then begin

    List.iter (BuildOCamlInstall.uninstall_by_name uninstall_state) targets;
    exit 0
  end;

  begin match !query_install_dir with
      None -> ()
    | Some p ->
      let open BuildOCamlInstall in
      List.iter (fun un ->
        if un.un_name = p then begin
          Printf.printf "%s\n%!" un.un_directory;
          exit 0
        end
      ) (BuildOCamlInstall.list_installed install_where);
      Printf.eprintf "Package %S is not installed\n%!" p;
      exit 2
  end;

  BuildOCamlConfig.set_global_config cout;

  (* Don't modify default values from now on, since they have been included
     in the default configuration ! *)

  let env_ocp_dirs = ref cin.cin_ocps_dirnames in
  let env_ocp_files = ref [] in
  if cin.cin_ocps_in_ocamllib then
    env_ocp_dirs := cfg.ocaml_ocamllib :: !env_ocp_dirs;

  time_step "Scanning env for .ocp files...";
  List.iter (fun dir ->
    if verbose 3 then
      Printf.eprintf "Scanning installed .ocp files in %S\n%!" dir;
    let dir = File.of_string dir in
    env_ocp_files := ( BuildOCP.scan_root dir) @ !env_ocp_files
  ) !env_ocp_dirs;
  time_step "   Done scanning env for .ocp files";
  let state = BuildOCP.init_packages () in
  let env_meta_dirs = ref cin.cin_meta_dirnames in

  if cin.cin_use_ocamlfind then begin
    let more_meta_dirs =
      match ocamlfind_path with
        [] -> [cfg.ocaml_ocamllib]
      | list -> list
    in
    env_meta_dirs := !env_meta_dirs @ more_meta_dirs;
  end;
  time_step "Loading METAs...";
  List.iter (fun dirname ->
    BuildOCamlMeta.load_META_files state cfg dirname
  ) !env_meta_dirs;

  time_step "   Done Loading METAs";

  time_step "Loading .ocp files from env...";

  let _nerrors1 =
    let config = BuildOCP.generated_config () in
    BuildOCP.load_ocp_files config state  !env_ocp_files
  in

  time_step "   Done Loading .ocp files from env";

  if !move_to_project then begin

      match project_dir with
      | None ->

        (* if we arrive here, it means we really needed ocp-build.root *)
        Printf.eprintf "Fatal error: no ocp-build.root file found.\n%!";
        Printf.eprintf
          "\tYou can use the -init option at the root of the project\n";
        Printf.eprintf "\tto create the initial file.\n%!";
        exit 2

      | Some project_dir ->

        Unix.chdir (File.to_string project_dir);
        Printf.fprintf stdout "ocp-build: Entering directory `%s'\n%!"
          (File.to_string project_dir);
        add_finally (fun () ->
          Printf.printf
            "ocp-build: Leaving directory `%s'\n%!" (File.to_string project_dir)
        );

        do_load_project_files cin project_dir state;

    end;

    (* [ocp-build configure] stops here, so it will not scan
       for .ocp files at this point. Instead, it will be done the
       first time the project is compiled, because [root_files] is
       empty. *)

    if !configure_arg then save_project := true;

    if !save_project then begin
      Printf.fprintf stderr "Updating ocp-build.root\n%!";
      BuildOptions.must_save_project ()
    end;


    if !conf_arg || !distrib_arg || !autogen_arg then exit 0;

    let use_digests = cin.cin_digest in

    if use_digests then BuildEngineMtime.use_digests true;

    time_step "Sorting packages...";
    let pj = BuildOCP.verify_packages state in

    time_step "   Done sorting packages";

    do_reply_to_queries pj;


    if !query_global then begin
      Printf.eprintf "Error: reached query-global end point.\n%!";
      exit 0
    end;

    BuildOptions.maybe_save ();

    if !configure_arg then exit 0;

    if !clean_arg then begin
      Printf.eprintf "Removing build target directory\n%!";

      BuildActions.delete_file_or_directory !build_dir_basename_arg;
      exit 0;
    end;

    do_print_project_info pj;


    match project_dir with
      None -> assert false
    | Some project_dir ->
    (*        let root_dir = File.dirname root_file in *)


      let b = do_init_project_building cfg project_dir pj in

      let projects =
        (* build the list of projects considered by the current command *)
        let projects = ref [] in
        match targets with
          [] ->
          StringMap.iter (fun _ pj ->
              projects := pj :: !projects) !packages_by_name;
          !projects
        | list ->
          List.iter (fun name ->
              try
                let pj = StringMap.find name !packages_by_name in
                projects := pj :: !projects
              with Not_found ->
                Printf.eprintf
                  "Error: Could not find target project %s\n%!" name;
                exit 2
            ) list;
          !projects
      in

      if !project_arg then exit 0;

      let install_what =

        let open BuildOCamlInstall in
        {
          install_asm_bin = true;
          install_byte_bin = true;
          install_asm_lib = true;
          install_byte_lib = true;
        }
      in

      if !uninstall_arg then begin

        List.iter (fun lib ->
          BuildOCamlInstall.uninstall uninstall_state lib)
          projects

      end (* !uninstall_arg *)
      else

      if !install_arg then begin

        do_install install_where install_what projects

      end (* !install_arg *)
      else begin

        let ncores = cin.cin_njobs in

        let ncores =
          if ncores < 1 then
            BuildConfig.number_of_cores () + 1
          else
            ncores
        in

        do_compile b cin ncores projects;

        if !tests_arg then begin

          do_test b ncores projects;
        end

      end




let finally () =
  List.iter (fun action -> action ()) !finally_do;
  time_step "End of execution";

  if !time_arg then begin
    Printf.printf "Time schedule:\n";
    List.iter (fun (msg, t1) ->
      Printf.printf "  %.2fs\t%s\n%!" (t1 -. t0) msg
    ) (List.rev !time_steps);
    Printf.printf "%!";
  end;
  ()

let _ =
  try
    Printexc.record_backtrace true;

    begin match initial_verbosity with None -> () | Some v ->
      set_verbosity v end;
    let targets = BuildArgs.parse_args () in

    if !init_arg && not (Sys.file_exists "ocp-build.root") then begin
      let oc = open_out "ocp-build.root" in
      close_out oc
    end;
    if !root_arg then exit 0;
    build targets;
    finally ()
  with ExitStatus n ->
    finally ();
    Pervasives.exit n
  | e ->
    let backtrace = Printexc.get_backtrace () in
    Printf.fprintf stderr "ocp-build: Fatal Exception %s\n%s\n%!"
      (Printexc.to_string e) backtrace;
    raise e
