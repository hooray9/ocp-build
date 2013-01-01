(*
  - Add missing labels when creating an array (Error)
*)

open ErrorLocation
open FixEmacs

let fix loc error_line next_lines =
  let indented_lines = FixUtils.find_indented [] next_lines in
  let (_, error_line) = OcpString.cut_at error_line ':' in
  let (_, labels) = OcpString.cut_at error_line ':' in
  let labels = String.concat " " (labels :: indented_lines) in
  Printf.fprintf stderr "labels: [%s]\n%!" labels;
  let labels = OcpString.split_simplify labels ' ' in

  let abs_begin_pos = loc.loc_begin_pos in
  let abs_end_pos = loc.loc_end_pos in
  Printf.fprintf stderr "Filename: %s\n%!"
    (File.to_string loc.loc_file.file_file);

  let expr = String.sub loc.loc_file.file_content
    abs_begin_pos (abs_end_pos - abs_begin_pos) in

  let rec find_indent expr begin_pos pos =
    match expr.[pos] with
        ' ' | '\t' | '{' | '(' -> find_indent expr (begin_pos+1) (pos+1)
      | _ -> begin_pos
  in
  let indent = find_indent expr (loc.loc_begin_pos - loc.loc_bol) 0 in

  let need_semicolon = ref true in
  let rec find_end expr brace abs_end_pos pos =
    match expr.[pos] with
        ' ' | '\t' | '\n' | '\r' -> find_end expr brace (abs_end_pos-1) (pos-1)
      | ';' -> need_semicolon := false; abs_end_pos
      | '}' when not brace ->
        find_end expr true (abs_end_pos-1) (pos-1)
      | _ when not brace ->
        find_end expr brace (abs_end_pos-1) (pos-1)
      | _ -> abs_end_pos
  in
  let abs_end_pos = find_end expr false abs_end_pos (String.length expr - 1) in

  Printf.fprintf stderr "Expression: <%s>\n%!" expr;

  let indent = String.make indent ' ' in

  FixUtils.(
    with_elisp
      [
        with_current_buffer loc.loc_file
          [
            insert_strings loc.loc_file (abs_end_pos+1)
              [
                (fun b ->
                  if !need_semicolon then Printf.bprintf b ";";
                  Printf.bprintf b "\n";
                  List.iter (fun label ->
                    Printf.bprintf b "%s%s = assert false;\n" indent label
  (*    Printf.bprintf b "%s%s = %s (* added *);\n" indent label label *)
                  ) labels;
                  Printf.bprintf b "%s" indent)
              ]
          ];
        print_message "Labels inserted, you need to complete it."
  ])
