open OcpSystem
open OcpLang

module EmacsLineParser : sig
  val parse_line : string -> string StringMap.t
  val extract_directory : string -> string

end = struct

  open Genlex

  let lexer = Genlex.make_lexer [ "-*-"; ":"; "-"; ";" ]

  let rec parse_tokens tokens =
    match tokens with
        Kwd "-*-" :: tail ->
          parse_options StringMap.empty [] tail
      | _ -> raise Parsing.Parse_error

  and parse_options map idents tokens =
    match tokens with
        Ident name :: Kwd "-" :: tail ->
          parse_options map (name :: idents) tail
      | Ident name :: Kwd ":" :: tail ->
        parse_value map (String.concat "-" (List.rev (name :: idents))) tail
      | Kwd "-*-" :: _ ->
        map
      | _ -> raise Parsing.Parse_error

  and parse_value map ident tokens =
    match tokens with
        String s :: tail ->
          let map = StringMap.add ident s map in
          parse_next map tail
      | Ident s :: tail -> (* TODO: use types to distinguish between strings and idents ? *)
        let map = StringMap.add ident s map in
        parse_next map tail
      | _ -> raise Parsing.Parse_error

  and parse_next map tokens =
    match tokens with
        Kwd ";" :: tail -> parse_options map [] tail
      | Kwd "-*-" :: tail -> map
      | _ -> raise Parsing.Parse_error

  let parse_line line =
    let tokens = Genlex.tokens_of_string lexer line in
    List.iter (fun token -> Printf.fprintf stderr "tok[%s]\n%!" (string_of_token token)) tokens;
    parse_tokens tokens

  let extract_directory s =
(* TODO: be more clever *)
    for i = 0 to String.length s - 1 do
      match s.[i] with
          '\'' -> s.[i] <- '"'
        | '`' -> s.[i] <- '"'
        | _ -> ()
    done;
    let tokens = Genlex.tokens_of_string lexer s in
    List.iter (fun token -> Printf.fprintf stderr "move:tok[%s]\n%!" (string_of_token token)) tokens;
    match tokens with
        [ Ident _; Ident "directory"; String s ] -> s
      | _ -> raise Parsing.Parse_error

end

let homedir = try Sys.getenv "HOME" with Not_found -> "/"

let expanse_directory name =
  if name = "" then "." else
    if name.[0] = '~' then
      Filename.concat homedir (String.sub name 1 (String.length name - 1))
    else name

let _ =
  Printf.eprintf "ocp-fix-errors started\n%!"

let error_file = Sys.argv.(1)
let line_pos = int_of_string Sys.argv.(2)


let directory dir_stack =
  match dir_stack with
      [] -> assert false
    | directory :: _ -> directory

let check_lines linenum dir_stack all_lines =
  match all_lines with
      [] -> ()
    | location_line :: lines ->
      Printf.fprintf stderr "%d: parse_location [%s]\n%!" linenum location_line;
      match try
              Some (
                ErrorLocation.parse_location
                  (directory dir_stack) location_line ) with
                  e ->
                    Printf.fprintf stderr "ERROR %s\n%!"
                      (Printexc.to_string e);
                    None
      with
          None ->
            Printf.fprintf stderr "Error Not found\n%!"

        | Some loc ->
          Printf.fprintf stderr "FOUND ERROR AT LINE POS %d (> %d ?)\n%!" linenum line_pos;
          if linenum > line_pos then begin
            FixEmacs.( with_elisp [ next_error ]);
            exit 0;
          end;
          Printf.fprintf stderr "TESTING\n%!";
          begin
            match lines with
                "Warning 8: this pattern-matching is not exhaustive." ::
                  "Here is an example of a value that is not matched:" ::
                  pattern_lines ->

                    Printf.fprintf stderr "Found a non-exhaustive pattern matching\n%!";
                    FixPattern.fix loc pattern_lines

              | error_line :: next_lines
                  when
                    OcpString.starts_with error_line
                      "Error: Some record field labels are undefined:"
                    ->
                Printf.fprintf stderr "Found an incomplete record\n%!";
                      FixRecord.fix loc error_line next_lines

              | error_line :: next_lines
                  when
                    OcpString.starts_with error_line "Error: The implementation"   ->
                Printf.fprintf stderr "Found a non-matching interface\n%!";
                      FixInterface.fix loc error_line next_lines

              | error_line :: next_lines when
                  OcpString.starts_with error_line
                    "Warning 26: unused variable"
                  ||
                    OcpString.starts_with error_line
                    "Warning 27: unused variable"
                  ->
                Printf.fprintf stderr "Found unused variable\n%!";
                    FixUnusedVariables.fix loc (directory dir_stack) next_lines

              | error_line :: next_lines when
                  OcpString.starts_with error_line
                    "Warning 12: this sub-pattern is unused"
                  ->
                Printf.fprintf stderr "Found unused pattern\n%!";
                    FixUnusedPatterns.fix loc (directory dir_stack) next_lines

              | error_line :: next_lines when
                  OcpString.starts_with error_line
                    "Error: This expression has type" ->
                Printf.fprintf stderr "Found un-expected type\n%!";
                      FixExpect.fix loc error_line next_lines

              |
                  "Error: This expression is not a function; it cannot be applied" :: _ ->
                Printf.fprintf stderr "Found a missing semi-colon\n%!";
                    FixSemi.fix loc

              | "Error: This function is applied to too many arguments;" ::
                  "maybe you forgot a `;'" :: _ ->
                Printf.fprintf stderr "Found a missing semi-colon (two many args)\n%!";
                    FixSemi.fix_line loc

              | _ ->
                ()
          end;
          exit 0

let rec skip_lines linenum dir_stack lines =
  match lines with
    | [] -> ()
    | line :: tail ->
      let (_, after) = String.cut_at line ':' in
      if OcpString.starts_with after " Entering directory" then
        let directory = EmacsLineParser.extract_directory after in
        skip_lines (linenum+1) (directory :: dir_stack) tail
      else
        if OcpString.starts_with after " Leaving directory" then
          let directory = EmacsLineParser.extract_directory after in
          match dir_stack with
              [] -> assert false
            | dir :: _ when dir <> directory -> assert false
            | _ :: dir_stack ->
              skip_lines (linenum+1) dir_stack tail
        else begin
          if linenum >= line_pos then
            check_lines linenum dir_stack lines;
          skip_lines (linenum+1) dir_stack  tail
        end

let main () =
  let lines = FileLines.of_file error_file in
  match lines with
      [] -> ()
    | first_line :: other_lines ->
      let map = EmacsLineParser.parse_line first_line in
      Printf.fprintf stderr "Options:\n%!" ;
      StringMap.iter (fun name value ->
        Printf.fprintf stderr "  [%s] --> [%s]\n%!" name value
      ) map;
      assert (StringMap.find "mode" map = "compilation");
      let directory = expanse_directory (StringMap.find "default-directory" map) in
      Printf.fprintf stderr "directory: %s\n" directory;

      skip_lines 2 [directory] other_lines

let _ =
  try
    main ()
  with e ->
    Printf.fprintf stderr "Fatal error: Exception %s\n%!" (Printexc.to_string e);
    exit 2
