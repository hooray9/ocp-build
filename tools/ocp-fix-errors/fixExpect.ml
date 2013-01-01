(*
 -  Replace x.(_) by x.[_] when x is a string (Error)
 -  Replace x.[_] by x.[_] when x is a string (Error)
*)


open Approx_lexer
open ErrorLocation
open FixEmacs

let rec find_terminator stack lexbuf =
  match Approx_lexer.token_pos lexbuf with
    | LPAREN, _ -> find_terminator (RPAREN :: stack) lexbuf
    | LBRACKET, _ -> find_terminator (RBRACKET :: stack) lexbuf
    | (RPAREN | RBRACKET) as paren, (begin_pos, _) ->
      begin
        match stack with
            prev :: stack ->
              assert (prev = paren);
              find_terminator stack lexbuf
          | [] -> paren, begin_pos
      end
    | _, _ -> find_terminator stack lexbuf

let replace file list =
  FixUtils.(
    with_elisp
      (
       [ find_file file ] @
        List.flatten (
          List.map (fun (begin_pos, len, str) ->
            [
              delete_region file (begin_pos+1) (begin_pos+1+len);
              insert_strings file (begin_pos+1) [
                (fun b -> Printf.bprintf b "%s" str)
              ];
            ]
          ) list) @
         [ save_current_buffer;
          print_message "[]/() substituted, file saved"
         ]
      )
  )

let fix_index file should_be_string abs_end_pos =
  Approx_lexer.init ();
  let lexbuf = Lexing.from_string file.file_content in
  Printf.fprintf stderr "abs_end_pos = %d\n%!" abs_end_pos;
  let rec iter () =
    let token = Approx_lexer.token_pos lexbuf in
    match token with
	(Approx_lexer.EOF,_) -> assert false
      | (token, (begin_pos, end_pos)) ->
        Printf.fprintf stderr "begin_pos = %d\n%!" begin_pos;
        if begin_pos >= abs_end_pos then begin
          Printf.fprintf stderr "AFTER: %s\n%!" (Approx_lexer.string_of_token token);
          assert (token = DOT);
          let (token, (begin_pos, end_pos)) = token_pos lexbuf in
          assert (token = LPAREN || token = LBRACKET);
          let (rtoken, end_pos) = find_terminator [] lexbuf in
          match token, rtoken with
            | LPAREN, RPAREN ->
              Printf.fprintf stderr "matching ()\n%!";
              if should_be_string then
                replace file [ (begin_pos, 1, "["); (end_pos, 1, "]") ]
            | LBRACKET, RBRACKET ->
              Printf.fprintf stderr "matching []\n%!";
              if not should_be_string then
                replace file [ (begin_pos, 1, "("); (end_pos, 1, ")") ]
            | _ -> assert false
              end else
  iter ()
 in
iter ()

let this_expression_has_type =  "This expression has type"
let but_an_expression_was_expected_of_type =
  "but an expression was expected of type"

open ErrorLocation

let fix loc error_line next_lines =
  let file = loc.loc_file in
  let abs_end_pos = loc.loc_end_pos in



  let indented_lines = FixUtils.find_indented [error_line] next_lines in
  let message = String.concat " " indented_lines in
  Printf.fprintf stderr "mismatch: [%s]\n%!" message;

  let before_begin_pos = OcpString.find this_expression_has_type message in
  let before_end_pos = before_begin_pos + String.length this_expression_has_type in
  let middle_begin_pos = OcpString.find_from but_an_expression_was_expected_of_type
    message before_end_pos in
  let middle_end_pos = middle_begin_pos + String.length but_an_expression_was_expected_of_type in

  let should_be = String.sub message before_end_pos (middle_begin_pos - before_end_pos) in
  let should_be = OcpString.unspace should_be in
  let instead_of = String.sub message middle_end_pos (String.length message - middle_end_pos) in
  Printf.fprintf stderr "should_be = [%s]\n%!" should_be;
  Printf.fprintf stderr "instead_of = [%s]\n%!" instead_of;
  let should_be = Approx_lexer.tokens_of_string should_be in
  let instead_of = Approx_lexer.tokens_of_string instead_of in

  if should_be = [ Approx_lexer.LIDENT "string" ] &&
    OcpList.last instead_of = Approx_lexer.LIDENT "array" then
    begin
      Printf.fprintf stderr "PROBABLY _.(_) instead of _.[_]\n%!";
      fix_index file true abs_end_pos;
      exit 0
    end
  else
  if instead_of = [ Approx_lexer.LIDENT "string" ] &&
    OcpList.last should_be = Approx_lexer.LIDENT "array" then
    begin
      Printf.fprintf stderr "PROBABLY _.[_] instead of _.(_)\n%!";
      fix_index file false abs_end_pos;
      exit 0
    end
  else begin
    match instead_of, should_be with
        [ Approx_lexer.LIDENT t1 ], [ Approx_lexer.LIDENT t2 ] ->
          begin match t1, t2 with
              "int", "float" ->
                Printf.fprintf stderr "cas 1\n%!";
                FixFloats.fix loc
            | "float", "int" ->

              Printf.fprintf stderr "cas 2\n%!";
              let s = String.sub file.file_content loc.loc_begin_pos
                (loc.loc_end_pos - loc.loc_begin_pos) in
              Printf.fprintf stderr "error on = [%s]\n%!" s;
              let s = Approx_lexer.tokens_of_string s in
              begin
                match s with
                    [ LIDENT _ ] ->
                      FixEmacs.(
                        with_elisp
                          [
                            find_file loc.loc_file;
                            insert_strings loc.loc_file (loc.loc_begin_pos+1)
                              [ fun b -> Buffer.add_string b "(float " ];
                            insert_strings loc.loc_file (loc.loc_end_pos+1)
                              [ fun b -> Buffer.add_string b ")" ];
                          ]
                      )
                  | _ -> ()
              end;
              FixFloats.fix loc
            | _ -> ()
          end
      | _ ->
        ()
  end;
  List.iter (fun token ->
    Printf.fprintf stderr "should_be: %s\n%!" (Approx_lexer.string_of_token token)
  ) should_be;

  List.iter (fun token ->
    Printf.fprintf stderr "instead_of: %s\n%!" (Approx_lexer.string_of_token token)
  ) instead_of;
