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
(* open Stdlib2 *)
open BuildOCPTree
open BuildOCPTypes
open SimpleConfig
open BuildOCPVariable

let verbose = DebugVerbosity.verbose ["B";"BP"] "BuildOCP"

type state = {
  validated : (string * string, package) Hashtbl.t;
  missing : (string * string, package list ref) Hashtbl.t;
}

type package_comparison =
  PackageEquality
| PackageConflict
| PackageUpgrade

let normalized_dir dir =
  File.to_string (File.of_string dir)

let compare_packages pk1 pk2 =
  let o1 = pk1.package_options in
  let o2 = pk2.package_options in
  let pk1_generated = get_bool_with_default o1 "generated" false in
  let pk2_generated = get_bool_with_default o2 "generated" false in
  match pk1_generated, pk2_generated with
    false, false ->
      PackageConflict
  | true, true ->
    if normalized_dir pk1.package_dirname = normalized_dir pk2.package_dirname &&
      pk1.package_type = pk2.package_type &&
      pk1.package_version = pk2.package_version
    (* TODO: We should also test for asm/byte... *)
    then begin
      if verbose 5 then
        Printf.eprintf "Discarding duplicated package %S\n%!" pk1.package_name;
      PackageEquality
    end
    else
      begin
        if verbose 5 then begin
          Printf.eprintf "Conflict over %S\n" pk1.package_name;
          Printf.eprintf "dirname: %S\n" pk1.package_dirname;
          Printf.eprintf "type: %S\n" (string_of_package_type pk1.package_type);
          Printf.eprintf "version: %S\n" pk1.package_version;
          Printf.eprintf "dirname: %S\n" pk2.package_dirname;
          Printf.eprintf "type: %S\n" (string_of_package_type pk2.package_type);
          Printf.eprintf "version: %S\n" pk2.package_version;
        end;
        PackageConflict
      end

  (* TODO: if we have the sources of a new version, we should probably
     accept the new version, but not accept any installed version
     depending on the old version.

     TODO: implement this in a different pass : we should filter out
     packages that are duplicated, especially to discard installed
     packages. But what if we have the sources of A, { A + B }
     installed, and not the sources of B ?

     In the meantime, the solution is to desinstall the installed
     versions when they conflict.  *)

  | true, false -> PackageConflict
  | false, true -> PackageConflict

let conflicts = ref []

let print_conflicts verbose_conflicts =
  if !conflicts <> [] then
  if verbose_conflicts then
    List.iter (fun (pk, pk2, pk3) ->
      Printf.eprintf "Warning: two projects called %S\n" pk.package_name;
      let print_package msg pk =
        let msg_len = String.length msg in
        let dirname_len = String.length pk.package_dirname in
        let filename_len = String.length pk.package_filename in
        if msg_len + dirname_len + filename_len > 70 then
          Printf.eprintf "  %s %s\n     (%s)\n" msg
            pk.package_dirname pk.package_filename
        else
          Printf.eprintf "  %s %s (%s)\n" msg
            pk.package_dirname pk.package_filename;
      in
      print_package "In" pk;
      print_package "In" pk2;
      print_package "Keeping" pk3;
    ) !conflicts
  else
    Printf.eprintf
      "Warning: %d package conflicts solved (use -print-conflicts)\n%!"
      ( List.length !conflicts )

let rec validate_project s pk =
  if verbose 5 then
    Printf.eprintf "validate_project: %s, tag=%s, id=%d\n" pk.package_name pk.package_tag pk.package_id;
  if pk.package_missing_deps = 0 then begin
    let key =  (pk.package_name, pk.package_tag) in

    let should_add =
      try
        let pk2 = Hashtbl.find s.validated key in

        match compare_packages pk pk2 with
        | PackageEquality ->
          (* same package, no need to add *)
          false
        | PackageUpgrade -> (* NOT YET DONE *)
          Hashtbl.remove s.validated key;
          true

        | PackageConflict ->

          let add_new =
            if pk.package_id > pk2.package_id then begin
              Hashtbl.remove s.validated key;
              true
            end else begin
              false
            end
          in
          conflicts := (pk, pk2,
            if add_new then pk else pk2
          ) :: !conflicts;
          add_new
      with Not_found -> true
    in

    if should_add then begin
      Hashtbl.add s.validated key pk;
      pk.package_validated <- true;

      try
        let list_ref = Hashtbl.find s.missing key in
        Hashtbl.remove s.missing key;
        List.iter (fun pk2 ->
	  pk2.package_missing_deps <- pk2.package_missing_deps - 1;
	  validate_project s pk2
        ) !list_ref;
      with Not_found -> ()
    end
  end

let is_enabled options =
  get_bool_with_default options "enabled" true

let check_project s pk =
  if is_enabled pk.package_options then begin

    pk.package_missing_deps <- 0;
    StringMap.iter (fun name pkdep ->
      if not pkdep.dep_optional then

      let key = (name, "") in (* TODO: we should use a datastructure that can handle
                                 dependencies by tag and by version *)
      if not (Hashtbl.mem s.validated key) then
	let list_ref =
	  try
	    Hashtbl.find s.missing key
	  with Not_found ->
	    let list_ref = ref [] in
	    Hashtbl.add s.missing key list_ref;
	    list_ref
	in
	list_ref := pk :: !list_ref;
	pk.package_missing_deps <- pk.package_missing_deps + 1
    ) pk.package_deps_map;
    validate_project s pk
  end



(*
val find_project : (File.t -> File.t)
*)
let find_root root_dir basenames =
  let rec find dirname (basenames : string list) =
    let file = File.add_basenames dirname basenames in
    if File.X.exists file then dirname else
      let new_dirname = File.dirname dirname in
      if new_dirname == dirname then raise Not_found;
      find new_dirname basenames
  in
  let root_dir = if File.is_absolute root_dir then root_dir else
      File.concat (File.X.getcwd ()) root_dir
  in
  find root_dir basenames

module PackageDepSorter = LinearToposort.Make(struct
  type t = package
  let node pd = pd.package_node
  let iter_edges f pd =
    List.iter (fun dep -> f dep.dep_project) pd.package_requires
  let name pd = pd.package_name
end)

module PackageLinkSorter = LinearToposort.Make(struct
  type t = package  package_dependency
  let node pd = pd.dep_project.package_node
  let iter_edges f pd1 =
    List.iter (fun pd2 ->
      if pd2.dep_link then f pd2) pd1.dep_project.package_requires
  let name pd = pd.dep_project.package_name
end)


(*
let dep_link dep =
  try
    match StringMap.find "link" dep.dep_options with
      OptionBool bool -> bool
    | _ ->
      Printf.fprintf stderr "Warning: option \"link\" is not bool !\n%!";
      false
  with Not_found -> false

let dep_syntax dep =
  try
    match StringMap.find "syntax" dep.dep_options with
      OptionBool bool -> bool
    | _ ->
      Printf.fprintf stderr "Warning: option \"syntax\" is not bool !\n%!";
      false
  with Not_found -> false
*)


let print_deps msg pk =
  Printf.eprintf "%s: Project %s depends on:\n%!" msg pk.package_name;
  List.iter (fun dep ->
    let pd = dep.dep_project in
    Printf.eprintf "\t%s %s%s%s%s\n%!"
      (string_of_package_type pd.package_type)
      pd.package_name
      (if dep.dep_link then "(link)" else "")
      (if dep.dep_syntax then "(syntax)" else "")
      (if dep.dep_optional then "(optional)" else "")
  ) pk.package_requires


let new_dep pk =
  {
    dep_project = pk;
    dep_link = false;
    dep_syntax = false;
    dep_optional = false;
  }

(* Do a closure of all dependencies for this project. Called only on
validated_projects *)
let update_deps pj =

  if verbose 5 then print_deps "BEFORE update_deps" pj;

  (*
    This computation depends on what we are dealing with:
    - For a syntax:
    - we must include all transitive link dependencies
    - for a Library dependency, we want to include all link dependencies
    - for a Syntax dependency, we want to also include syntax dependencies

    - For a library, all syntax dependencies should remain syntax
    dependencies. Link dependencies of syntaxes should become syntax
    dependencies.

    For now, we have three kinds of dependencies:
    1) 'link' dependencies: we must copy all 'link' transitive dependencies
    as new 'link' dependencies.
    2) 'syntax' dependencies: we must copy all 'link' transitive dependencies
    as new 'syntax' dependencies.
    3) neither 'link' nor 'syntax': we should not copy transitive dependencies.

    We cannot do it in one pass: we should first compute strong dependencies, and
    remove packages not meeting strong dependencies. Then, we can redo the
    computation, this time knowing which optional packages are available.

  *)

  let deps = Hashtbl.create 111 in
  let list = ref [] in

  (* Keep only one copy of a package, with all the flags merged
     pj.package_requires <-
     List.filter (fun dep ->
     let pd = dep.dep_project in
     try
     let dep2 = Hashtbl.find deps pd.package_id in
     dep2.dep_link <- dep2.dep_link || dep.dep_link;
     dep2.dep_syntax <- dep2.dep_syntax || dep.dep_syntax;
     dep2.dep_optional <- dep2.dep_optional && dep.dep_optional;
     false
     with Not_found ->
     Hashtbl.add deps dep.dep_project.package_id dep;
     list := dep :: !list;
     true
     ) pj.package_requires;
  *)

  let new_dep pj2 =
    try
      Hashtbl.find deps pj2.package_id
    with Not_found ->
      let dep = new_dep pj2 in
      Hashtbl.add deps pj2.package_id dep;
      list := dep :: !list;
      dep
  in

  let is_syntax =
    match pj.package_type with
      SyntaxPackage  -> true
    | LibraryPackage
    | ProgramPackage
    | ObjectsPackage
    | TestPackage
    | RulesPackage
      -> false
  in
  List.iter (fun dep ->
    if dep.dep_link then
      match dep.dep_project.package_type with
      | SyntaxPackage
      | ProgramPackage ->
        if is_syntax then
          dep.dep_link <- true
        else begin
          dep.dep_syntax <- true;
          dep.dep_link <- false;
        end
      | LibraryPackage
      | ObjectsPackage
      | RulesPackage
        -> ()
      | TestPackage ->
        Printf.eprintf "Error: Test %S appears in requirements of %S\n%!"
          dep.dep_project.package_name
          pj.package_name;
          exit 2;
  ) pj.package_requires;

  (* add all link dependencies, transitively *)
  let rec add_link_deps pj =
    List.iter (fun dep ->
      let pj2 = dep.dep_project in
      let dep2 = new_dep pj2 in
      if verbose 5 then
        Printf.eprintf "%S -> %S\n" pj.package_name pj2.package_name;
      if dep.dep_link && not dep2.dep_link then begin
        dep2.dep_link <- true;
        if verbose 5 then
          Printf.eprintf "%S -> link %S\n" pj.package_name pj2.package_name;
        add_link_deps pj2
      end
    ) pj.package_requires
  in
  add_link_deps pj;

    (* add syntax dependencies, and their link dependencies
       transformed into syntax dependencies *)
  let rec add_link_as_syntax_deps pj =
    List.iter (fun dep ->
      if dep.dep_link then
        let pj2 = dep.dep_project in
        let dep2 = new_dep pj2 in
        if not dep2.dep_syntax then begin
          if verbose 5 then
            Printf.eprintf "%S -> syntax %S\n" pj.package_name pj2.package_name;
          dep2.dep_syntax <- true;
          add_link_as_syntax_deps pj2
        end
    ) pj.package_requires
  in

  let add_syntax_deps pj =
    List.iter (fun dep ->
      if dep.dep_syntax then
        let pj2 = dep.dep_project in
        let dep2 = new_dep pj2 in
        if not dep2.dep_syntax then begin
          dep2.dep_syntax <- true;
          if verbose 5 then
            Printf.eprintf "%S -> syntax %S\n" pj.package_name pj2.package_name;
          add_link_as_syntax_deps pj2;
        end
    ) pj.package_requires
  in
  add_syntax_deps pj;



  (*
    let rec add_link_deps to_set dep =
    if dep.dep_link then
    let pj2 = dep.dep_project in
    let dep2 = try
    Hashtbl.find deps pj2.package_id
    with Not_found ->
    let dep = {
    dep_project = pj2;
    dep_link = false;
    dep_syntax = false;
    dep_optional = false;
    } in
    Hashtbl.add deps pj2.package_id dep;
    list := dep :: !list;
    dep
    in
    to_set dep2;
    match pj2.package_type with
    | LibraryPackage
    | ObjectsPackage ->
    List.iter (add_link_deps to_set) pj2.package_requires;
    | ProgramPackage -> ()
    in

    let rec add_dep dep =
    let pj2 = dep.dep_project in
    match pj2.package_type with
    | LibraryPackage
    | ObjectsPackage ->
    if dep.dep_link then
    List.iter
    (add_link_deps (fun dep -> dep.dep_link <- true))
    pj2.package_requires;
    (* TODO: why dep.dep_syntax <- false HERE ? If we want to
    generalize to "tags", we would probably need to tell how to
    build the closure of these tags. For example, the "link" tag
    means that we should keep in the closure all the next deps
    with also the "link" tag, but unset their "syntax" tag. But,
    for the "syntax" tag, we only want to keep in the closure the
    deps with the "link" tag, after changing it to "syntax" !  *)

    if dep.dep_syntax then
    List.iter
    (add_link_deps (fun dep -> dep.dep_syntax <- false))
    pj2.package_requires;
    | ProgramPackage -> ()
    in
    List.iter add_dep pj.package_requires;
  *)
  pj.package_requires <- !list;

  (*  TODO: do better
      List.iter (fun pd ->
      List.iter add_dep pj.package_requires
      ) pj.package_requires; *)

  if verbose 5 then print_deps "AFTER update_deps SORT" pj;

  (*
    (* TODO: verify this is useless ? since sorted later again *)
    pj.package_requires <- (*PackageLinkSorter.sort sort_sorted *) !list;

    if verbose 5 then print_deps "AFTER update_deps SORT" pj;
  *)
  ()


let reset_package_ids array =
  for i = 0 to Array.length array - 1 do
    array.(i).package_id <- i
  done

(*
val load_packages : (project -> int)
*)
(* Note that files should be sorted from the most internal one to
the deepest ones. *)

let init_packages () =

  let packages = BuildOCPInterp.initial_state () in
  packages

let empty_config () = BuildOCPInterp.empty_config (* !BuildOCPVariable.options *)
let generated_config () =
  BuildOCPInterp.generated_config (* !BuildOCPVariable.options *)

let load_ocp_files global_config packages files =

(*
  let pj =
  {
   (*
 project_config = config_file;
    project_file = file_t;
    project_dir = File.dirname file_t;
   *)
    project_files = files;
    project_packages = IntMap.empty;
    project_npackages = 0;
    project_disabled = [];
    project_incomplete = [];
    project_sorted = [];
    project_missing = [];
  }
  in
*)


(*  let config = BuildOCPInterp.empty_config !BuildOCPVariable.options in *)

  let nerrors = ref 0 in

  let rec iter parents files =
    match files with
	[] -> ()
      | file :: next_files ->
	match parents with
	    [] -> assert false
	  | (parent, config) :: next_parents ->
            let file = File.to_string file in
	    if OcpString.starts_with file parent then
	      let dirname = Filename.dirname file in
	      if verbose 5 then
	        Printf.eprintf "Reading %s with context from %s\n%!" file parent;
	      let config =
		try
		  BuildOCPInterp.read_ocamlconf packages config file
		with BuildMisc.ParseError ->
		  incr nerrors;
		  config
	      in
	      iter ( (dirname, config) :: parents ) next_files
	    else
	      iter next_parents files
  in
  iter [ "", global_config ] files;
  !nerrors

let requires_keep_order_option = new_bool_option "requires_keep_order" false

let verify_packages packages =
  let packages = BuildOCPInterp.final_state packages in

  let state = {
    missing = Hashtbl.create 111;
    validated = Hashtbl.create 111;
  }
  in

  Array.iter (fun pk -> check_project state pk) packages;

  let project_incomplete = ref [] in
  let project_disabled = ref [] in

  Array.iter (fun pk ->
    if is_enabled pk.package_options then begin
      if pk.package_missing_deps > 0 then
	project_incomplete := pk :: !project_incomplete
    end else
      project_disabled := pk :: !project_disabled
  ) packages;

  let list = ref [] in
  Hashtbl.iter (fun _ pk ->
    list := pk :: !list;

    pk.package_requires <- [];
    StringMap.iter (fun dep_name dep ->
      try
        let pd = Hashtbl.find state.validated (dep_name, "") in
        pk.package_requires <- { dep with
          dep_project = pd } :: pk.package_requires
      with Not_found -> () (* probably an optional dependency *)
    ) pk.package_deps_map;

  ) state.validated;

  let project_missing = ref [] in
  Hashtbl.iter (fun (name, _) list_ref ->
    project_missing := (name, !list_ref) :: !project_missing)
    state.missing;
(* Note that the result of this function can contain more elements
  as the initial list, as new dependencies are automatically added. *)

  List.iter (fun pk ->

    begin
    (* a list of packages that should appear before this package *)
    let is_after = get_strings_with_default pk.package_options "is_after"  []
    in
    List.iter (fun name ->
      try
        let pk2 = Hashtbl.find state.validated (name, "") in
        let found = ref false in
        List.iter (fun dep ->
          if dep.dep_project == pk2 then found := true;
        ) pk.package_requires;
        if not !found then
          pk.package_requires <- new_dep pk2 :: pk.package_requires;
      with Not_found -> ()
    ) is_after;
    end;

begin
    (* a list of packages that should appear after this package *)
    let is_before = get_strings_with_default pk.package_options "is_before" []
    in
    List.iter (fun name ->
      try
        let pk2 = Hashtbl.find state.validated (name, "") in
        let found = ref false in
        List.iter (fun dep ->
          if dep.dep_project == pk then found := true;
        ) pk2.package_requires;
        if not !found then
          pk2.package_requires <- new_dep pk :: pk2.package_requires;
      with Not_found -> ()
    ) is_before;
end;
  ) !list;

  let (project_sorted, cycle, other) =
    PackageDepSorter.sort !list
  in
  let _list = () in
  List.iter update_deps project_sorted;

  List.iter (fun pk ->
    (* TODO: now that we know which package is available, we should
       set flags before processing file attributes. *)
    pk.package_files <- (try get_local pk.package_options "files" with Not_found -> []);
    pk.package_tests <- (try get_local pk.package_options "tests" with Not_found -> []);
  ) project_sorted;

  let npackages = Array.length packages in

  let pj = {
(*    project_files = files; *)
    project_sorted = Array.of_list project_sorted;
    project_missing = !project_missing;
    project_disabled = Array.of_list !project_disabled;
    project_incomplete = Array.of_list !project_incomplete;
  } in
(* TODO: fix this assertion. The equality stands only if we count
also duplicated packages. *)
  assert (npackages >=
      Array.length pj.project_sorted +
        Array.length pj.project_incomplete +
        Array.length pj.project_disabled);
  reset_package_ids pj.project_sorted;
  (* TODO: The impact of this is that all dependencies are sorted in
     the same order in all packages. This might, however, not be what
     someone wants, because you might want to have a different link
     order than the one globally inferred.  *)
  Array.iter (fun pk ->
    if requires_keep_order_option.get pk.package_options  then
      let (sorted, cycle, _ ) = PackageLinkSorter.sort pk.package_requires in
      assert (cycle = []);
      pk.package_requires <- sorted
    else
      pk.package_requires <- List.sort (fun dep1 dep2 ->
        compare
          dep1.dep_project.package_id
          dep2.dep_project.package_id) pk.package_requires;

    if verbose 9 then begin
      Printf.eprintf "Package %S\n" pk.package_name;
      List.iter (fun dp ->
        Printf.eprintf "\t%S%s%s\n"
          dp.dep_project.package_name
          (if dp.dep_link then " (link)" else "")
          (if dp.dep_syntax then " (syntax)" else "");
      ) pk.package_requires
    end;
  ) pj.project_sorted;

  reset_package_ids pj.project_incomplete;
  reset_package_ids pj.project_disabled;

  pj

(*
  val save_project : (File.t -> (project -> unit))
  let save_project file_t pj =
  save_with_help pj.project_config
*)


let scan_root root_dir =
  let files = ref [] in
  BuildScanner.scan_directory_for_suffix
    (File.to_string root_dir) ".ocp" (fun filename ->
    match (Filename.basename filename).[0] with
    'a'..'z' | 'A'..'Z' | '0'..'9' ->
      files := File.of_string filename :: !files
    | _ -> ());
  List.rev !files
(* files are reverted, so that the first in breadth are used first
(this is expected from [load_project] *)

let magic_head = "OCP-"
let magic_head_len = String.length magic_head
let magic_kind = "PROJ"
let magic_kind_len = String.length magic_kind
let magic_version = "20120928"
let magic_version_len = String.length magic_version
let magic = magic_head ^ magic_kind ^ magic_version
let magic_len = String.length magic

let save_project_state state filename =
  let oc = File.X.open_out_bin filename in
  output_string oc magic;
  output_value oc (state : project);
  close_out oc

let load_project_state filename =
  let ic = File.X.open_in_bin filename in
  let possible_magic = String.create magic_len in
  begin try
          really_input ic possible_magic 0 magic_len;
    with e ->
      close_in ic;
      failwith "load_project_state: truncated file"
  end;
  if possible_magic <> magic then begin
    close_in ic;
    if String.sub possible_magic 0 magic_head_len <> magic_head then
      failwith "load_project_state: not an OCP file";
    if String.sub possible_magic magic_head_len magic_kind_len
      <> magic_kind then
      failwith "load_project_state: not an OCP PROJECT file";
    if String.sub possible_magic (magic_head_len + magic_kind_len)
      magic_version_len <> magic_version then
      failwith "load_project_state: bad OCP PROJECT version";
  end;
  try
    let v = (input_value ic : project) in
    close_in ic;
    v
  with e ->
    close_in ic;
    raise e

(*
val scan_project : (project -> unit)
let scan_project pj =
  let files = ref [] in
  BuildScanner.scan_directory_for_suffix
    (File.to_string pj.project_dir) ".ocp" (fun filename ->
    files := File.of_string filename :: !files);
  pj.project_files =:= List.rev !files;
  save_project pj.project_file pj;
  ()
*)

(*

  if !list_ocp_files || !verbosity_arg > 1 then begin
    Printf.eprintf "Found %d project files:\n%!" (List.length !files);
    List.iter (fun file ->
      Printf.eprintf "\t%s\n%!" file) !files;
  end;

*)

let find_package pj file =
  let list = ref [] in

  let st = File.X.lstat file in
(*
  let dir_t = pj.project_dir in
  let _dir = File.to_string dir_t in
*)
  let check_file pk filename =
    let file = File.of_string (Filename.concat pk.package_dirname filename) in
    try
      let st2 = File.X.lstat file in
      if
        st.Unix.st_ino = st2.Unix.st_ino &&
        st.Unix.st_dev = st2.Unix.st_dev then
        list := pk :: !list
    with _ -> ()
  in
  Array.iter (fun pk ->
    List.iter (fun (filename, _) ->
      check_file pk filename;
      let (kernel, extension) = File.cut_last_extension filename in
      match extension with
        | "ml" -> check_file pk (filename ^ ".mli")
        | "mli" -> ()
        | "mll" -> check_file pk (filename ^ ".ml")
        | "mly" ->
          check_file pk (filename ^ ".ml");
          check_file pk (filename ^ ".mli")
        | _ -> ()
    ) pk.package_files
  ) pj.project_sorted;

  !list

let rec find_obuild f dir =
  let possible_dir = Filename.concat dir "_obuild" in
  if Sys.file_exists possible_dir then
    f possible_dir
  else
    let new_dir = Filename.dirname dir in
    if dir <> new_dir then find_obuild f new_dir
