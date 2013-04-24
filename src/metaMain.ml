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

let _ =
  for i = 1 to Array.length Sys.argv - 1 do
    let file = Sys.argv.(i) in
    try
      Printf.fprintf stderr "Parsing file %S\n%!" file;
      let (meta : MetaTypes.meta) = MetaParser.parse_file file in
      let name = match meta.meta_name with
          None -> Filename.basename (Filename.dirname file)
        | Some name -> name in
      MetaFile.create_meta_file (Printf.sprintf "META.%s" name) meta
    with e ->
      Printf.fprintf stderr "MetaMain: Exception %S while parsing %S\n%!"
        (Printexc.to_string e) file
  done
