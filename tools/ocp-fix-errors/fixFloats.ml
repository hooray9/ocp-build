
open ErrorLocation
open Approx_lexer

let rec to_float loc tokens b indent =
  match tokens with
      [] -> ()
    | (token, (begin_pos, end_pos)) :: tokens ->
      begin
        match token with
            INT _
          | STAR | PLUS | MINUS | INFIXOP3 "/"
            ->
              FixEmacs.(
                insert_strings loc.loc_file (end_pos+1) [ fun b -> Buffer.add_char b '.' ] b indent
              )
          | _ -> ()
      end;
      to_float loc tokens b indent

let to_float loc tokens =
  let tokens = List.rev tokens in
  List.iter (fun (token, _) ->
    Printf.fprintf stderr "FLOAT: %s\n%!" (Approx_lexer.string_of_token token)
  ) tokens;

  FixEmacs.(
    with_elisp
      [
        find_file loc.loc_file;
        to_float loc tokens;
        save_current_buffer
      ])

let fix loc =

  let file = loc.loc_file in
  let lexbuf = Lexing.from_string file.file_content in
  Approx_lexer.init ();

  let rec iter tokens =
    let token = Approx_lexer.token_pos lexbuf in
    match token with
      | (
          (
            INT _ | FLOAT _ | LIDENT _
          )
            , _) ->
          iter_operator  (token :: tokens)

      | (
          (
               STAR | INFIXOP3 "*." | INFIXOP3 "**"
              | PLUS | INFIXOP2 "+."
              | MINUS | INFIXOP2 "-."
              | INFIXOP3 "/" | INFIXOP3 "/."
          )
          , _) ->
          iter_value  (token :: tokens)

      | (_ , (_, bpos)) ->
        if bpos > loc.loc_begin_pos then begin
          to_float loc tokens
        end else
          iter  []

  and iter_operator tokens =
    let token = Approx_lexer.token_pos lexbuf in
    match token with
      | (
          (
               STAR | INFIXOP3 "*." | INFIXOP3 "**"
              | PLUS | INFIXOP2 "+."
              | MINUS | INFIXOP2 "-."
              | INFIXOP3 "/" | INFIXOP3 "/."
          )
          , _) ->
          iter_value  (token :: tokens)

      | (_ , (_, bpos)) ->
        if bpos > loc.loc_begin_pos then begin
          to_float loc tokens
        end else
          iter  []

  and iter_value tokens =
    let token = Approx_lexer.token_pos lexbuf in
    match token with
      | (
          (
            INT _ | FLOAT _ | LIDENT _
          )
            , _) ->
          iter_operator  (token :: tokens)

      | (_ , (_, bpos)) ->
        if bpos > loc.loc_begin_pos then begin
          to_float loc tokens
        end else
          iter  []


  in
  iter []
