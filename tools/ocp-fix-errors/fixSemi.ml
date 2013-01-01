(*
  - Adds a ; at the end of the expression, when it seems applied to another argument. (Error)
*)

open ErrorLocation
open FixEmacs

let fix loc =
  let abs_end_pos = loc.loc_end_pos in
  FixUtils.(
    with_elisp
      [
        find_file loc.loc_file;
        insert_strings loc.loc_file (abs_end_pos+1)
              [
                (fun b -> Printf.bprintf b ";")
              ];
        save_current_buffer;
        print_message "Semi-colon inserted, file saved"
      ])

let fix_line loc =
  let pos = loc.loc_end_pos in
  let file = loc.loc_file in
  let s = file.file_content in
  let len = String.length s in
  let rec iter pos0 pos =
    if pos < len then
      match s.[pos] with
          '\r' | '\n' -> pos0
        | ' ' | '\t' -> iter pos0 (pos+1)
        | _ -> iter (pos+1) (pos+1)
    else
      len
  in
  let abs_end_pos = iter pos pos in
  FixUtils.(
    with_elisp
      [
        find_file loc.loc_file;
        insert_strings loc.loc_file (abs_end_pos+1)
              [
                (fun b -> Printf.bprintf b ";")
              ];
        save_current_buffer;
        print_message "Semi-colon inserted, file saved"
      ])

