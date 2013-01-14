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

type install_info = {
  install_libdir : string;
  install_bindir : string;
}

type install_what = {
  install_byte_bin : bool;
  install_asm_bin : bool;
  install_byte_lib : bool;
  install_asm_lib : bool;
}

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

let install install_info install_what lib =

  if not lib.lib_installed then begin

    assert (lib.lib_name <> "");
    let installdir = Filename.concat install_info.install_libdir lib.lib_name in
    let installbin = install_info.install_bindir in
    let meta_file = Filename.concat installdir "META" in
    if Sys.file_exists meta_file then begin
      Printf.fprintf stderr "Install error: %S already exists\n%!" meta_file;
    end else
      begin
        Printf.fprintf stderr "Installing %S in %S:%!" lib.lib_name installdir;
        BuildMisc.safe_mkdir installdir;

        let meta = MetaFile.empty () in
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
              meta.meta_exists_if <- Some file.file_basename;
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
              Some (Filename.concat installbin file.file_basename)
            | RUN_BYTE when
                install_what.install_byte_bin ->
              Some (Filename.concat installbin file.file_basename)

            | RUN_BYTE
              | CMI
            | CMO
            | CMX
            | CMXS
            | CMA
            | CMXA
            | CMXA_A
            | C_A
            | RUN_ASM
               -> None

          in
          match dst_file with
            None -> ()
          | Some dst_file ->
            let src_file = file_filename file in
            Printf.fprintf stderr " %s%!" file.file_basename;
            if not !fake_install then
              File.RawIO.copy_file src_file dst_file
        in
        List.iter (fun (file, kind) ->
          install_file file kind
        ) lib.lib_byte_targets;
        List.iter (fun (file, kind) ->
          install_file file kind
        ) lib.lib_asm_targets;
        Printf.fprintf stderr "\n%!";

        if meta.meta_archive <> StringMap.empty then begin
          Printf.fprintf stderr "Generating META file %s\n%!" meta_file;
          if not !fake_install then
            MetaFile.create_meta_file meta_file meta
          else
            MetaFile.create_meta_file (Printf.sprintf "/tmp/META.%s" lib.lib_name) meta
        end
      end
  end

