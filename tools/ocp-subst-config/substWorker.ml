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

(*
  Substitute strings from a config file.
*)

open StringSubst

let output_arg = ref None
let output_suffix_arg = ref None
let inplace_arg = ref false

let config_file_arg = ref None
let replace_string_arg1 = ref []
let replace_string_arg2 = ref []

let rec find_config_file dirname basename =
  let filename = Filename.concat dirname basename in
  if Sys.file_exists filename then filename else
    let new_dirname = Filename.dirname dirname in
    if new_dirname <> dirname then
      find_config_file new_dirname basename
    else begin
      Printf.fprintf stderr "Could not find config file %s\n%!" basename;
      exit 2
    end

let load_config_file config_file =
  (*  Printf.fprintf stderr "load_config_file %s\n" config_file; *)
  let c = empty_subst () in
  File.iter_lines (fun line ->
    if String.length line > 0 && line.[0] <> '#' then
      let (key, v) = OcpString.cut_at line '=' in
    (*    Printf.fprintf stderr "[%s] = [%s]\n" key v; *)
      add_to_subst c key v
  ) config_file;
  c

let subst_in_file recursive config source_file dest_file =
  let s = File.string_of_file source_file in
  let nsubst,s =
     if recursive then
       iter_subst config s
     else
       subst config s
  in
  begin match dest_file with
    None ->
      Printf.printf "%s%!" s
   | Some dest_file ->
    let oc = open_out dest_file in
    output_string oc s;
    close_out oc
  end;
  nsubst

let arg_anon filename =
  let config =
    match !config_file_arg with
    | None ->
        let config = empty_subst () in
        List.iter2 (add_to_subst config)
          !replace_string_arg1 !replace_string_arg2;
        config
    | Some filename ->
        let config_file =
          if Filename.is_implicit filename then
            find_config_file
              (Filename.dirname filename) filename
          else filename in
        load_config_file config_file
  in
  let dest_file =
    match !inplace_arg, !output_arg, !output_suffix_arg with
      true, None, None -> Some filename
    | true, _, _ ->
      Printf.eprintf "Error: -i(--inplace) is incompatible with other output arguments.\n%!";
      exit 2
    | _, None, None -> None
    | _, Some _, Some _ ->
      Printf.eprintf "Error: -o (--output) and -s (--suffix) are incompatible.\n%!";
      exit 2
    | _, Some "-", _ -> None
    | _, Some filename, _ -> Some filename
    | _, _, Some suffix ->
      if Filename.check_suffix filename suffix then
        Some (Filename.chop_suffix filename suffix)
      else begin
        Printf.eprintf
            "Error: filename %S has no suffix %S (use -o)\n%!" filename suffix;
          exit 2
        end
  in
  ignore (subst_in_file true config filename dest_file)

let replace_string =
  [
    Arg.String (fun s -> replace_string_arg1 := s :: !replace_string_arg1);
    Arg.String (fun s -> replace_string_arg2 := s :: !replace_string_arg2);
  ]

let arg_list = Arg.align [
  "-config-file", Arg.String (fun s -> config_file_arg := Some s ),
  " <config> : name of config file";

  "--output", Arg.String (fun s -> output_arg := Some s), "";
  "-o", Arg.String (fun s -> output_arg := Some s),
  " <filename> : name of file to generate (- for stdout)";

  "-i", Arg.Set inplace_arg, "";
  "--inplace", Arg.Set inplace_arg, " : replace in file";

  "-s", Arg.String (fun s -> output_suffix_arg := Some s), "";
  "--suffix", Arg.String (fun s -> output_suffix_arg := Some s),
  " SUFFIX : replace in file and remove suffix ('.in' for example)";

  "--replace-string", Arg.Tuple replace_string, "";
  "-str", Arg.Tuple replace_string, " STR REPL : replace string STR by REPL";

(*  "-subst", Arg.String arg_anon, " <file> : substitute in file";*)
]

let arg_usage = Printf.sprintf "%s [OPTIONS] files" Sys.argv.(0)
