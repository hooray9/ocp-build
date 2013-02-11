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

open MetaTypes
open BuildTypes
open BuildEngineTypes
open BuildEngineGlobals
open BuildOCPTypes
open BuildOCPVariable

(* TODO:
  When installing, we should accept the option -sanitize, to clean everything
  before, i.e. remove META files that would be refer to the same package.
  We should check that there is nothing remaining before installing, and
  ask the user to use -sanitize if necessary to remove conflicting files.
*)


(*
   When to generate META files ?
   - 1st easy: when we install files into one of the ocamlfind directories
        The META file is in the directory, without "directory"
   - 2nd easy: when we install files into OCAMLLIB subdirectories
        The META. file is in one of the ocamlfind directories, and
        the "directory" starts with "^"
   - 3rd : when we install files into somewhere else
        The META. file is in one of the ocamlfind directory, and
        the "directory" is absolute.
*)

type install_where = {
  install_libdirs : string list;
  install_bindir : string;
  install_datadir : string option;

  install_ocamlfind : string list;
  install_ocamllib : string;
}

type install_what = {
  install_byte_bin : bool;
  install_asm_bin : bool;
  install_byte_lib : bool;
  install_asm_lib : bool;
}

type package_uninstaller = {
  mutable un_nfiles : int;
  mutable un_ndirs : int;
  mutable un_version : string;
  mutable un_name : string;
  mutable un_descr : string;
  mutable un_warning : string option;
  mutable un_directory : string;
  mutable un_type : string;
  mutable un_packages : string list;
}

type kind = DIR | FILE | VERSION | WARNING | DESCR | TYPE | PACK

type log = (kind * string) list


module List = struct
  include List

  let rec split_after l1 l2 =
    match l1, l2 with
      _, [] -> Some l1
    | [], _ -> None
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then
        split_after t1 t2
      else
        None

  let starts_with l1 l2 =
    (split_after l1 l2) <> None

end

let split_dir dir =
  let rec iter pos pos0 path dir len =
    if pos = len then begin
      if pos = pos0 then List.rev path else
        List.rev (String.sub dir pos0 (pos-pos0) :: path)
    end else
      match dir.[pos] with
        '/' | '\\' ->
          let path =
            if pos = pos0 then path
            else
              String.sub dir pos0 (pos-pos0) :: path
          in
          let pos = pos+1 in
          iter pos pos path dir len
      | _ -> iter (pos+1) pos0 path dir len
  in
  iter 0 0 [] dir (String.length dir)

let string_of_kind = function
| CMI -> "CMI"
| CMO -> "CMO"
| CMX -> "CMX"
| CMXS -> "CMXS"
| CMA -> "CMA"
| CMXA -> "CMXA"
| CMXA_A -> "CMXA_A"
| C_A -> "C_A"
| RUN_BYTE -> "RUN_BYTE"
| RUN_ASM -> "RUN_ASM"

let fake_install = ref false


let add_log log kind name =
  log := (kind, name) :: !log

let rec safe_mkdir log filename =
  try
    let st = Unix.stat filename in
    match st.Unix.st_kind with
      Unix.S_DIR -> ()
    | _ ->
      failwith (Printf.sprintf
                  "File.safe_mkdir: %S exists, but is not a directory"
                  filename)
  with Unix.Unix_error (Unix.ENOENT, _, _) ->
    let dirname = Filename.dirname filename in
    safe_mkdir log dirname;
    let basename = Filename.basename filename in
    match basename with
    | "." | ".." -> ()
    | _ ->
      Unix.mkdir filename 0o755;
      add_log log DIR filename

(* [dst] must be the target file name, not the name of its directory *)
let rec copy_rec log src dst =
    (*    Printf.eprintf "copy_rec: %S -> %S\n%!" src dst; *)
  let st = Unix.stat src in
  match st.Unix.st_kind with
  | Unix.S_DIR ->
    safe_mkdir log dst;
    File.RawIO.iter_dir (fun basename ->
      copy_rec log (Filename.concat src basename)
        (Filename.concat dst basename)) src
  | Unix.S_REG ->
    add_log log FILE dst;
    File.RawIO.copy_file src dst;
    Unix.chmod dst st.Unix.st_perm

  | _ ->
    failwith (Printf.sprintf
                "File.copy_rec: cannot copy unknown kind file %S"
                src)

let install_or_uninstall log src_file dst_file =
  if not !fake_install then begin
    Printf.fprintf stderr " %s%!" (Filename.basename dst_file);
    copy_rec log src_file dst_file
  end

(* TODO: we should do an analysis on the packages that are going to be
  installed, to check that all libraries have also their dependencies
  loaded. *)

let save_uninstall_log uninstall_file log =
  let oc = open_out uninstall_file in
  Printf.fprintf oc "OCP 1\n";
  List.iter (fun (kind, file) ->
    Printf.fprintf oc "%s %s\n" (match kind with
      FILE -> "REG"
    | DIR -> "DIR"
    | VERSION -> "VER"
    | WARNING -> "WAR"
    | DESCR -> "LOG"
    | TYPE -> "TYP"
    | PACK -> "PCK"
    ) (String.escaped file);
  ) log;
  close_out oc

(* TODO: in another version, we could actually link bundle packages to
other packages, and use uninstall instructions from these packages *)

let install_bundle install_where name logs installdir =
  let log = ref [] in
  let uninstall_file = Filename.concat installdir
    (Printf.sprintf "%s.uninstall" name) in
  if not !fake_install then begin
    if not (Sys.file_exists installdir) then
      safe_mkdir log installdir;
    add_log log FILE uninstall_file;
  end;
  let log = !log in
  let log = List.map (fun name -> (PACK,name)) logs @ log in
  let log = (TYPE, "bundle") :: log in
  save_uninstall_log uninstall_file log

let install install_where install_what lib installdir =
  let log = ref [] in
  let uninstall_file = Filename.concat installdir
    (Printf.sprintf "%s.uninstall" lib.lib_name) in
  let save_uninstall warning =
    if not !fake_install then
      let log = !log in
      let log =
        (VERSION, lib.lib_version) ::
          (TYPE, BuildOCPTree.string_of_package_type lib.lib_type) ::
          log in
      let log = match warning with
          None -> log
        | Some warning -> (WARNING, warning) :: log
      in
      save_uninstall_log uninstall_file log
  in
  try
    let installbin = install_where.install_bindir in
    if not !fake_install then begin
      if not (Sys.file_exists installdir) then
        safe_mkdir log installdir;
      add_log log FILE uninstall_file;
    end;

  (* Do the installation *)
    let meta = MetaFile.empty () in

    meta.meta_version <- Some lib.lib_version;
    meta.meta_description <- Some
      (string_option_with_default lib.lib_options "description" lib.lib_name);
    List.iter (fun dep ->
      if dep.dep_link then
        MetaFile.add_requires meta [] [dep.dep_project.lib_name]
    ) lib.lib_requires;

    let install_file file kind =
      let dst_file =
        match kind with

        | CMI when
            install_what.install_asm_lib || install_what.install_byte_lib ->
          Some (Filename.concat installdir file.file_basename)
        | C_A when
            install_what.install_asm_lib || install_what.install_byte_lib ->
          Some (Filename.concat installdir file.file_basename)
        | CMO when
            install_what.install_byte_lib ->
          Some (Filename.concat installdir file.file_basename)
        | CMX
        | CMXA_A when
            install_what.install_asm_lib ->
          Some (Filename.concat installdir file.file_basename)
        | CMA when
            install_what.install_byte_lib ->
          MetaFile.add_archive meta [ "byte", true ] [ file.file_basename ];
              meta.meta_exists_if <- file.file_basename::
                meta.meta_exists_if;
              Some (Filename.concat installdir file.file_basename)
        | CMXA when
            install_what.install_asm_lib ->
          MetaFile.add_archive meta [ "native", true ] [ file.file_basename ];
              Some (Filename.concat installdir file.file_basename)
        | CMXS when
            install_what.install_asm_lib ->
          Some (Filename.concat installdir file.file_basename)
        | RUN_ASM when
            install_what.install_asm_bin ->
          Some (Filename.concat installbin
                  (Filename.chop_suffix file.file_basename ".asm"))
        | RUN_BYTE when
            install_what.install_byte_bin ->
          Some (Filename.concat installbin file.file_basename)

        | RUN_BYTE
        | RUN_ASM
        | CMI
        | CMO
        | CMX
        | CMXS
        | CMA
        | CMXA
        | CMXA_A
        | C_A
          -> None

      in
      match dst_file with
        None -> ()
      | Some dst_file ->

        if not !fake_install then begin
          let dirname = Filename.dirname dst_file in
          if not (Sys.file_exists dirname) then
            safe_mkdir log dirname
        end;

      (*            Printf.fprintf stderr "\tto %S : %!" dst_file; *)
        let src_file = file_filename file in
        install_or_uninstall log src_file dst_file
    in
    Printf.eprintf "\tfiles: %!";
    List.iter (fun (file, kind) ->
      install_file file kind
    ) lib.lib_byte_targets;
    List.iter (fun (file, kind) ->
      install_file file kind
    ) lib.lib_asm_targets;
    Printf.fprintf stderr "\n%!";

    begin match  install_where.install_datadir with
      None -> ()
    | Some datadir ->
      let datadir = Filename.concat datadir lib.lib_name in

      List.iter (fun file ->
        safe_mkdir log datadir;
        let basename = Filename.basename file in
        let dst_file = Filename.concat datadir basename in
        let src_file = Filename.concat (File.to_string lib.lib_dirname) file in
        install_or_uninstall log src_file dst_file
      )
        (list_option_with_default lib.lib_options "data_files" []);

    end;


    List.iter (fun file ->
      safe_mkdir log installdir;
      let basename = Filename.basename file in
      let dst_file = Filename.concat installdir basename in
      let src_file = Filename.concat (File.to_string lib.lib_dirname) file in
      install_or_uninstall log src_file dst_file
    )
      (list_option_with_default lib.lib_options "lib_files" []);

    List.iter (fun file ->
      safe_mkdir log installbin;
      let basename = Filename.basename file in
      let dst_file = Filename.concat installbin basename in
      let src_file = Filename.concat (File.to_string lib.lib_dirname) file in
      install_or_uninstall log src_file dst_file
    )
      (list_option_with_default lib.lib_options "bin_files" []);

  (* What kind of META file do we create ? *)
    let topdir_list = split_dir (Filename.dirname installdir) in
    let ocamlfind_path = List.map split_dir install_where.install_ocamlfind in

    let meta_files =
      if List.mem topdir_list ocamlfind_path then
        [Filename.concat installdir "META"]
      else
        let ocamllib = split_dir install_where.install_ocamllib in
        let installdir_list = split_dir installdir in
        match List.split_after installdir_list ocamllib with
        | None ->
          meta.meta_directory <- Some installdir;
          []
        | Some subdir ->
          meta.meta_directory <- Some ("^" ^ String.concat "/" subdir);
          []
    in
    let rec iter meta_files =
      match meta_files with
        [] ->
          Printf.eprintf
            "Warning: could not find a correct location to write\n";
          Printf.eprintf "\tthe META file\n%!"

      | meta_file :: meta_files ->
        try
          if !fake_install then
            Printf.fprintf stderr "Generating META file %s\n%!" meta_file
          else begin
            MetaFile.create_meta_file meta_file meta;
            add_log log FILE meta_file;
            Printf.fprintf stderr "Generated META file %s\n%!" meta_file;
          end
        with _ -> iter meta_files
    in
    iter (if meta_files = [] then
        let meta_basename = Printf.sprintf "META.%s" lib.lib_name in
        List.map (fun dirname ->
          Filename.concat dirname meta_basename
        ) (install_where.install_ocamlfind @ [ install_where.install_ocamllib ])
      else meta_files);

    save_uninstall None;

  with e ->
    save_uninstall (Some "Install partially failed");
    raise e

(* TODO: we might install the same package several times in different
   directories, no ? *)

let find_installdir install_where install_what lib_name =

    (* Check whether it is already installed : *)
  let rec iter possible libdirs =
    match libdirs with
      [] ->
        begin
          match possible with
            None ->
              Printf.eprintf "Error: no directory where to install files\n%!";
              None
          | Some installdir ->
            Some installdir
        end

    | libdir :: libdirs ->
      let installdir = Filename.concat libdir lib_name in
      if Sys.file_exists installdir then (* Found ! *)
        begin
            (* TODO: shouldn't we check for an .uninstall file ? *)
          Printf.eprintf "Error: package %S seems already installed in\n"
            lib_name;
          Printf.eprintf "\t%S\n%!" installdir;
          None
        end
      else
        begin
          match possible with
          | None ->
            if
              (try
                 File.RawIO.safe_mkdir installdir;
                 true
               with _ -> false)
            then begin
              Unix.rmdir installdir;
              iter (Some installdir) libdirs
            end else
              iter None libdirs
          | Some _ ->
            iter possible libdirs
        end
  in
  iter None install_where.install_libdirs

let uninstallers = Hashtbl.create 13

let rec scan_for_uninstallers files libdirs =
  match libdirs with
    [] -> files
  | libdir :: libdirs ->
    let files = ref files in
    BuildScanner.scan_directory_for_suffix libdir ".uninstall"
      (fun filename ->
(*        Printf.eprintf "UINSTALLER %S\n%!" filename; *)
        let name = Filename.chop_suffix (Filename.basename filename)
          ".uninstall" in
        files := StringMap.add name filename !files);
    scan_for_uninstallers !files libdirs

let load_uninstallers install_where =
  try
    Hashtbl.find uninstallers install_where.install_libdirs
  with Not_found ->
    let files = scan_for_uninstallers StringMap.empty
      install_where.install_libdirs in
    Hashtbl.add uninstallers install_where.install_libdirs files;
    files

let find_uninstaller install_where lib_name =
  let uninstallers = load_uninstallers install_where in
  try
    Some (StringMap.find lib_name uninstallers)
  with Not_found -> None

(*
  let uninstaller = Printf.sprintf "%s.uninstall" lib_name in
  let rec iter files =
    match files with
      [] -> None
    | uninstall_file :: files ->
      (*      Printf.eprintf "CHECK %S\n%!" uninstall_file; *)
      if Filename.basename uninstall_file = uninstaller then
        Some uninstall_file
      else iter files
  in
  StringMap.iter uninstallers
*)

let rec uninstall_by_uninstaller uninstall_file =
  let lib_name = Filename.chop_suffix (Filename.basename uninstall_file)
    ".uninstall"
  in
    let list = File.lines_of_file uninstall_file in
    List.iter (fun line ->
      match OcpString.cut_at line ' ' with
      | "OCP", _ -> ()
      | "REG", file ->
        if Sys.file_exists file then
          Sys.remove file
      | "DIR", file ->
        if Sys.file_exists file then
          Unix.rmdir file
      | "VER", version -> ()
      | "WAR", warning -> ()
      | "LOG", log -> ()
      | "TYP", kind -> ()
      | "PCK", filename ->
        if Sys.file_exists filename then
          uninstall_by_uninstaller filename
      | _ ->
        Printf.eprintf "Bad line [%S] in file %S\n%!" line uninstall_file;
    ) list;
    Printf.printf "Package %s uninstalled\n%!" lib_name

let rec uninstall_by_name install_where lib_name =
  match find_uninstaller install_where lib_name with
    None ->
      Printf.eprintf
        "Warning, uninstall of %s failed: could not find uninstaller file %S\n%!"
        lib_name (lib_name ^ ".uninstall");
  | Some uninstall_file ->
    uninstall_by_uninstaller uninstall_file

let uninstall install_where lib =
  if not lib.lib_installed
    && bool_option_with_default lib.lib_options
      "install" true
  then
    uninstall_by_name install_where lib.lib_name

let load_uninstaller filename =
    let list = File.lines_of_file filename in
    let name = Filename.chop_suffix (Filename.basename filename) ".uninstall" in
    let un = {
      un_nfiles = 0;
      un_ndirs = 0;
      un_version = "n/a";
      un_name = name;
      un_descr = name;
      un_warning = None;
      un_directory = Filename.dirname filename;
      un_type = "n/a";
      un_packages = [];
    } in
    List.iter (fun line ->
      match OcpString.cut_at line ' ' with
      | "OCP", _ -> ()
      | "REG", file -> un.un_nfiles <- 1 + un.un_nfiles
      | "DIR", file -> un.un_ndirs <- 1 + un.un_ndirs
      | "VER", v -> un.un_version <- v
      | "WAR", w -> un.un_warning <- Some w
      | "LOG", d -> un.un_descr <- d
      | "TYP", t -> un.un_type <- t
      | "PCK", pck -> un.un_packages <- pck :: un.un_packages
      | _ ->
        Printf.eprintf "Bad line [%S] in file %S\n%!" line filename;
    ) list;
    un

let list_installed install_where =
  let uninstallers = load_uninstallers install_where in
  let list = ref [] in
  StringMap.iter (fun _ filename ->
    list := load_uninstaller filename :: !list) uninstallers;
  !list

let is_installed install_where lib_name =
  (find_uninstaller install_where lib_name) <> None
