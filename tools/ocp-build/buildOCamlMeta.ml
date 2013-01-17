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
open BuildConfig.TYPES

let load_META_files pj cfg top_dirname =
  Printf.eprintf "Loading METAs from %S\n%!" top_dirname;
  BuildScanner.scan_directory (fun meta_dirname basename meta_filename ->
    (*
      Printf.eprintf "dirname=%S\n%!" dirname;
      Printf.eprintf "basename=%S\n%!" basename;
      Printf.eprintf "filename=%S\n%!" filename;
    *)
    if OcpString.starts_with basename "META" then
      try
        let meta = MetaParser.parse_file meta_filename in
        Printf.eprintf "Loaded %S\n%!" meta_filename;

        let rec add_meta meta_dirname pj path name meta =
          Printf.eprintf "add_meta %S %S\n%!" path name;

          let dirname = match meta.meta_directory with
            | Some dirname when dirname <> "" ->
              if dirname.[0] = '^' || dirname.[0] = '+' then
                Filename.concat cfg.ocaml_ocamllib
                  (String.sub dirname 1 (String.length dirname-1))
              else
                if Filename.is_relative dirname then
                  Filename.concat meta_dirname dirname
                else
                  dirname
            | _ -> meta_dirname
          in

          Printf.eprintf "dirname=%S\n%!" dirname;
          let exists =
            match meta.meta_exists_if with
              [] -> true
            | list ->
              List.for_all (fun filename ->
                let proof_filename = Filename.concat dirname filename in
                if not (Sys.file_exists proof_filename) then begin
                  Printf.eprintf
                    "Warning: proof of package %S does not exist\n%!"
                    proof_filename;
                  false
                end else true) list
          in
          if exists then
          (*
            let name =
            match meta.meta_name with
            TODO: meta_name is the archive name !!
            None -> name
            | Some name ->  (* lowercase to handle 'camlimages' *)
            String.lowercase name
            in  *)
          let fullname = path ^ name in
          let has_asm = ref false in
          let asm_archive = ref "" in
          let has_byte = ref false in
          let byte_archive = ref "" in
          StringMap.iter (fun _ var ->
            match var.metavar_preds, var.metavar_value with
              (* TODO: handle multiple files (objects) *)
              [ "byte", true ], [ archive ]
                when Filename.check_suffix archive ".cma"
                  ->
                    has_byte := true;
                    byte_archive := Filename.chop_suffix archive ".cma"
            | [ "native", true ], [ archive ]
              when Filename.check_suffix archive ".cmxa"
                ->
              has_asm := true;
                  asm_archive := Filename.chop_suffix archive ".cmxa"

            | _ -> ()
          ) meta.meta_archive;

          let archive = match !has_asm, !has_byte with
              false, false -> None
            | true, true ->
              if !asm_archive = !byte_archive then
                Some !byte_archive
              else begin
                Printf.eprintf "Warning: no common name for asm and byte in %S\n%!" fullname;
                None
              end
            | true, _ -> Some !asm_archive
            | _ , true -> Some !byte_archive
          in

          let requires = ref [] in
          StringMap.iter (fun _ var ->
            match var.metavar_preds with
            | [] -> requires := var.metavar_value

                (*
                  | [ "byte", true ] ->
                  has_byte := Some var.metavar_value
                  | [ "native", true ] ->
                  has_asm := Some var.metavar_value
                *)
            | _ -> ()
          ) meta.meta_requires;

              (* for objects, we should set   pk.package_sources <- source_files; *)

          let pk = BuildOCPInterp.new_package pj fullname dirname
            meta_filename BuildOCPTree.LibraryPackage in

          pk.package_source_kind <- "meta";
          List.iter (fun s ->
            let ( dep :  'a package_dependency) =
              BuildOCPInterp.new_package_dep pk s in
            dep.dep_link <- true
          ) !requires;

              (* this package has already been generated *)
          pk.package_options <- StringMap.add "generated"
            (OptionBool true) pk.package_options;

          begin
            match archive with
              None ->
                pk.package_options <- StringMap.add "meta"
                  (OptionBool true) pk.package_options;
                Printf.eprintf "Warning: package %S is meta\n%!" fullname
            | Some archive ->
              pk.package_options <- StringMap.add "archive"
                (OptionList [archive]) pk.package_options;
          end;
          List.iter (fun (name, meta) ->
            add_meta dirname pj (fullname ^ ".") name meta) meta.meta_package;

        in
        let name = MetaParser.name_of_META meta_filename in
        add_meta meta_dirname pj "" name meta

      with e ->
        Printf.eprintf "Warning: exception %S while loading %S\n%!"
          (Printexc.to_string e) meta_filename

  ) top_dirname
