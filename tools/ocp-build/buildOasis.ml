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

open BuildOCPTree
open BuildOCPVariable

let find_indent line =
  let rec find_indent line i len =
    if i < len then
      match line.[i] with
          ' ' | '\t' -> find_indent line (i+1) len
        | '#' -> (i, true)
        | _ -> (i, false)
    else (i, false)
  in
  let len = String.length line in
  let (indent, comment) = find_indent line 0 len in
  (indent, comment, String.sub line indent (len - indent))

(* From oasis-0.2.1~alpha1%  less src/oasis/OASISRecDescParser.ml LGPL *)
let oasis_lexer = Genlex.make_lexer
  [
        (* Statement *)
    "+:"; "$:"; ":"; "if"; "{"; "}"; "else";
        (* Section *)
    "Flag"; "Library"; "Executable";
    "SourceRepository"; "Test";
    "Document";
        (* Expression *)
    "!"; "&&"; "||"; "("; ")"; "true"; "false"
  ]

type oasis_line =
    Line of string * oasis_line list ref

let read_oasis filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    let rec read_line stack ic =
      let line = input_line ic in
      let (indent, comment, line) = find_indent line in
      if comment then
        read_line stack ic
      else
        push_line stack ic indent line

    and push_line stack ic indent line =
      match stack with
          [] -> assert false
        | (current_indent, lines) :: previous_stack ->
            if indent = current_indent then begin
              lines := Line (line, ref []) :: !lines;
              read_line stack ic
            end else
              if indent < current_indent then
                push_line previous_stack ic indent line
              else (* indent > current_indent *)
                match !lines with
                    [] -> assert false
                  | Line (previous_line, new_lines) :: _ ->
                    new_lines := Line (line, ref []) :: !new_lines;
                    let stack = (indent, new_lines) :: stack in
                    read_line stack ic
    in
    read_line [(0, lines)] ic
  with End_of_file ->
    close_in ic;
    !lines

let print_oasis lines =
  let rec print indent lines =
    List.iter (fun line ->
      let Line (s, lines) = line in
      Printf.fprintf stderr "%s%s\n" indent s;
      print (indent ^ "___") !lines
    ) (List.rev lines)
  in
  print "" lines;
  Printf.fprintf stderr "%!"

let merge_line content lines =
  let lines = content :: (List.map (function Line (s, _) -> s) (List.rev !lines)) in
  String.concat " " lines


let split_words s =
  let orig = String.copy s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\r' | '\n' | '\t' | ';' | ',' ->
      s.[i] <- ' '
    | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_'
    | ' ' | '.'
      ->
      ()
    | _ ->
      Printf.kprintf failwith "Illegal word in %S\n%!" orig
  done;
  OcpString.split_simplify s ' '

let split_words_lowercase s =
  let orig = String.copy s in
  for i = 0 to String.length s - 1 do
    match s.[i] with
    | '\r' | '\n' | '\t' | ';' | ',' ->
      s.[i] <- ' '
    | 'A'..'Z' -> s.[i] <- Char.lowercase s.[i]
    | 'a'..'z' | '0'..'9' | '_'
    | ' ' | '.'
      ->
      ()
    | _ ->
      Printf.kprintf failwith "Illegal word in %S\n%!" orig
  done;
  OcpString.split_simplify s ' '

type oasis_package = {
  opk_filename : string;
  opk_name : string;
  opk_type : BuildOCPTree.package_type;
  opk_dirname : string;
  opk_modules : string list;
  opk_internal_modules : string list;
  opk_build_depends : string list;
  opk_main_is : string list;
  opk_install : bool;
}

let parse_package opk lines =
  Printf.fprintf stderr "parse_package %s\n%!" opk.opk_name;
  let open Genlex in
  try
    let opk_modules = ref [] in
    let opk_internal_modules = ref [] in
    let opk_build_depends = ref [] in
    let opk_dirname = ref opk.opk_dirname in
    let opk_main_is = ref [] in
    let opk_install = ref true in
    List.iter (fun line ->
      let Line (s, lines) = line in
      let (header, content) = OcpString.cut_at s ':' in
      match String.lowercase header with

      | "mainis" ->
        let line = merge_line content lines in
        Printf.eprintf  "[%s] MainIs = %S\n%!" opk.opk_name line;
        let modules = split_words line in
        List.iter (fun s -> Printf.eprintf "MODULE %S\n" s) modules;
        Printf.eprintf "\n%!";
        opk_main_is := !opk_main_is @ modules

      | "modules" ->
        let line = merge_line content lines in
        Printf.eprintf  "[%s] FILES = %S\n%!" opk.opk_name line;
        let modules = split_words line in
        List.iter (fun s -> Printf.eprintf "MODULE %S\n" s) modules;
        Printf.eprintf "\n%!";
        opk_modules := !opk_modules @ modules

      | "internalmodules" ->
        let line = merge_line content lines in
        Printf.fprintf stderr "[%s] FILES = %s\n%!" opk.opk_name line;
        let modules = split_words line in
        opk_internal_modules := !opk_internal_modules @ modules

      | "path" ->
        let line = merge_line content lines in
        Printf.fprintf stderr "[%s] DIRNAME = %s\n%!" opk.opk_name line;
        begin match split_words line with
          [ subdir ] -> opk_dirname := Filename.concat !opk_dirname subdir
          | _ ->
            failwith "Error 'path'\n%!";
        end

      | "install" ->
        let line = merge_line content lines in
        Printf.fprintf stderr "[%s] Install = %s\n%!" opk.opk_name line;
        begin match split_words_lowercase line with
          [ "true" ] -> opk_install := true
          | [ "false" ] -> opk_install := false
          | _ ->
            failwith "Error 'install'\n%!";
        end

      | "builddepends" ->
        let line = merge_line content lines in
        Printf.fprintf stderr "[%s] REQUIRES = %s\n%!" opk.opk_name line;
        opk_build_depends := !opk_build_depends @ split_words line

      | _ ->
        Printf.fprintf stderr "[%s]Discarding line [%s]\n%!" opk.opk_name s
    ) (List.rev lines);
    List.iter (fun s -> Printf.eprintf "MODULE1 %S\n" s) !opk_modules;
    let opk = {
      opk with
      opk_modules = !opk_modules;
      opk_internal_modules = !opk_internal_modules;
      opk_build_depends = !opk_build_depends;
      opk_dirname = !opk_dirname;
      opk_main_is = !opk_main_is;
      opk_install = !opk_install;
    }
    in
    List.iter (fun s -> Printf.eprintf "MODULE2 %S\n" s) opk.opk_modules;
    Some opk
  with Failure s ->
    Printf.eprintf "Warning: in package %S, error:\n" opk.opk_name;
    Printf.eprintf "  %s\n%!" s;
    None

let empty_opk = {
  opk_filename = "";
  opk_name = "";
  opk_dirname = "";
  opk_type = LibraryPackage;
  opk_modules = [];
  opk_internal_modules = [];
  opk_build_depends = [];
  opk_main_is = [];
  opk_install = true;
}

let parse_oasis opk_filename lines =
  let open Genlex in
  let empty_opk = {
    empty_opk with
    opk_filename;
    opk_dirname = Filename.dirname opk_filename;
  } in
  let project_name = ref "" in
  let opks = ref [] in
  List.iter (fun line ->
    let Line (s, lines) = line in
    try
      let tokens = OcpGenlex.tokens_of_string oasis_lexer s in
      let opk =
        match tokens with
          [ Ident "Name" ; Kwd ":" ; (String name | Ident name) ] ->
          project_name := name;
          None
        | [ Kwd "Library"; (String name | Ident name) ] ->
          let opk_name =
            if name = !project_name then name
            else Printf.sprintf "%s.%s" !project_name name in
          let opk = {
            empty_opk with
            opk_name;
            opk_type = LibraryPackage;
          } in
          parse_package opk !lines
        | [ Kwd "Executable"; (String name | Ident name) ] ->
          let opk_name =
            if name = !project_name then name
            else Printf.sprintf "%s.%s" !project_name name in
          let opk_name = opk_name ^ "-command" in
          let opk = {
            empty_opk with
            opk_name;
            opk_type = ProgramPackage;
          } in
          parse_package opk !lines
        | _ -> None
      in
      match opk with
        None -> ()
      | Some opk ->
        opks := opk :: !opks
    with _ ->
      Printf.fprintf stderr "Discarding line [%s]\n%!" s
  ) (List.rev lines);
  !opks

open BuildOCPTypes

let load_project pj filename =
  let lines = read_oasis filename in
  print_oasis lines;
  let opks = parse_oasis filename lines in

  let package_options =
    List.fold_left (fun vars f -> f vars)
      StringMap.empty !BuildOCPVariable.options
  in
  let package_options = StringMap.add "sort" (OptionBool true)
      package_options in

  List.iter (fun opk ->
    Printf.eprintf "opk_name = %S\n%!" opk.opk_name;
    List.iter (fun s -> Printf.eprintf "  module: %S\n%!" s) opk.opk_modules;
    if opk.opk_modules <> [] || opk.opk_internal_modules <> [] ||
    opk.opk_main_is <> [] then
    let pk = BuildOCPInterp.new_package pj opk.opk_name opk.opk_dirname
        opk.opk_filename opk.opk_type in
    pk.package_source_kind <- "oasis";
    List.iter (fun s ->
      let ( dep :  'a package_dependency) =
        BuildOCPInterp.new_package_dep pk s in
      dep.dep_link <- true
    ) opk.opk_build_depends;

    let external_options = [] in
    let internal_options =
      (OptionBoolSet ("install", false)) ::
        external_options in
    pk.package_raw_files <-
      List.map (fun s -> (s, external_options)) opk.opk_modules @
      List.map (fun s -> (s, internal_options)) opk.opk_internal_modules @
      List.map (fun s -> (s, external_options)) opk.opk_main_is;
      List.iter (fun (s, _) ->
        Printf.eprintf "SOURCE %S\n%!" s) pk.package_raw_files;

  let package_options = StringMap.add "install" (OptionBool opk.opk_install)
      package_options in

    pk.package_options <- package_options
  ) opks;
  0


