open ErrorLocation

let with_elisp list =
  let b = Buffer.create 1111 in
  List.iter (fun f -> f "" b) list;
  let s = Buffer.contents b in
  Printf.fprintf stderr "%s%!" s;
  Printf.printf "%s%!" s

let with_current_buffer file actions indent b =
  Printf.bprintf b "%s(with-current-buffer (find-file-noselect \"%s\")\n"
    indent (File.to_string file.file_file);
  begin
    let indent = indent ^ "  " in
    List.iter (fun f -> f indent b) actions
  end;
  Printf.bprintf b ")\n"

let goto_char pos indent b =
  Printf.bprintf b "%s(goto-char %d)\n" indent pos

let delete_region file begin_pos end_pos indent b =
  let begin_pos = final_pos file begin_pos in
  let end_pos = final_pos file end_pos in
  Printf.bprintf b "%s(delete-region %d %d)\n" indent begin_pos end_pos;
  add_edition file (Deletion (begin_pos, end_pos - begin_pos))

let insert_strings file pos strings indent b =
  let bb = Buffer.create 100 in
  List.iter (fun f -> f bb) strings;
  let s = Buffer.contents bb in
  let pos = final_pos file pos in
  goto_char pos indent b;
  Printf.bprintf b "%s(insert \"%s\")\n" indent s;
  add_edition file (Insertion (pos, String.length s));
  ()

let find_file file indent b =
  Printf.bprintf b "%s(find-file \"%s\")\n" indent
    (File.to_string file.file_file)

let save_current_buffer indent b =
  Printf.bprintf b "%s(save-buffer)\n" indent

let next_error indent b =
  Printf.bprintf b "%s(next-error)\n" indent

let print_message msg indent b =
  Printf.bprintf b "%s(message \"%s\")\n" indent msg
