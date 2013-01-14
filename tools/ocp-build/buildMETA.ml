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

module TYPES = struct

  type meta = {
    mutable meta_version : string option;
    mutable meta_description : string option;
    mutable meta_requires : (string * string list ref) list;
    mutable meta_archive : (string * string list ref) list;
    mutable meta_exists_if : string option;
    mutable meta_package : (string * meta) list;
  }

end

open TYPES

let add_requires meta list values =
  let s = String.concat ", " (List.sort compare list) in
  try
    let old_values = List.assoc s meta.meta_requires in
    old_values := !old_values @ values
  with Not_found ->
    meta.meta_requires <- meta.meta_requires @
      [ s, ref values ]

let add_archive meta list values =
  let s = String.concat ", " (List.sort compare list) in
  try
    let old_values = List.assoc s meta.meta_archive in
    old_values := !old_values @ values
  with Not_found ->
    meta.meta_archive <- meta.meta_archive @
      [ s, ref values ]

let fprintf_option_field oc indent name field =
    match field with
      None -> ()
    | Some s ->
      Printf.fprintf oc "%s%s = %S\n" indent name s

let fprintf_entries oc indent name entries =
  List.iter (fun (cases, values) ->
    Printf.fprintf oc "%s%s%s = %S\n"
      indent name
      (if cases = "" then "" else
          Printf.sprintf "(%s)" cases)
      (String.concat "," !values)
  ) entries

let create_meta_file filename meta =
  let oc = open_out filename in
  let rec fprintf_meta oc indent meta =
    fprintf_option_field oc indent "version" meta.meta_version;
    fprintf_option_field oc indent "description" meta.meta_description;
    fprintf_entries oc indent "requires" meta.meta_requires;
    fprintf_entries oc indent "archive" meta.meta_archive;
    fprintf_option_field oc indent "exists_if" meta.meta_exists_if;
    List.iter (fun (name, meta) ->
      Printf.fprintf oc "%spackage %S (\n" indent name;
      fprintf_meta oc (indent ^ "  ") meta;
      Printf.fprintf oc "%s)\n" indent;
    ) meta.meta_package
  in
  fprintf_meta oc "" meta;
  close_out oc
