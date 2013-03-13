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

open BuildEngineTypes

let verbose =
  DebugVerbosity.add_submodules "B" [ "BED" ];
  DebugVerbosity.verbose [ "BED" ] "BuildEngineDisplay"

let need_escape =
  try
    ignore (Sys.getenv "TERM"); true
  with Not_found ->
    match Win32.os_type with
      Win32.WINDOWS | Win32.CYGWIN -> true
    | Win32.UNIX -> false

let term_escape s =
  if need_escape then String.escaped s else s

let color = ref (Unix.isatty Unix.stderr)
let columns =
  let with_process_in cmd f =
    let ic = Unix.open_process_in cmd in
    try
      let r = f ic in
      ignore (Unix.close_process_in ic) ; r
    with exn ->
        ignore (Unix.close_process_in ic) ; raise exn
  in
  try
    (* terminfo *)
    with_process_in "tput cols"
      (fun ic -> ref (int_of_string (input_line ic)))
  with Unix.Unix_error _ | End_of_file | Failure _ -> try
    (* GNU stty *)
    with_process_in "stty size"
      (fun ic ->
        match OcpString.split (input_line ic) ' ' with
        | [_ ; v] -> ref (int_of_string v)
        | _ -> failwith "stty")
  with Unix.Unix_error _ | End_of_file | Failure _ -> try
    (* shell envvar *)
    ref (int_of_string (Sys.getenv "COLUMNS"))
  with Not_found | Failure _ ->
      ref 80

let init _b = ()

let string_limit n s =
  let len = String.length s in
  if len <= n then s else
    let s' = if n >= 0 then String.sub s 0 n else "" in
    if n >= 3 then String.blit "..." 0 s' (n-3) 3;
    s'

let pretty_rule_name rule len =
  let f = rule.rule_main_target.file_file in
  let base = File.basename f in
  let dir = File.to_string (File.dirname f) in
  let dir =
    try (* rm first directory (_obuild/) *)
      let i = String.index dir Filename.dir_sep.[0] in
      String.sub dir (i+1) (String.length dir - i - 1)
    with Not_found | Invalid_argument "String.sub" -> dir
  in
  let curlen = String.length base + String.length Filename.dir_sep in
  let dir = string_limit (len - curlen) dir in
  let curlen = curlen + String.length dir in
  let pad = if len > curlen then String.make (len - curlen) ' ' else "" in
  if !color then
    String.concat ""
      ([ dir; Filename.dir_sep;
         "\027[1m"; File.basename (File.chop_extension f); "\027[m.";
         String.concat "." (File.extensions f) ]
       @ [ pad ])
  else
    String.concat "" [dir; Filename.dir_sep; base; pad]

let print_stat_line b proc =
  if !color then begin
    let npar = List.length b.build_stats_running_rules in
    let current_rules =
      string_limit (!columns - 13 - npar)
        (String.concat " "
           (List.rev_map
              (fun (r,_) -> (Hashtbl.find b.build_rules r).rule_main_target.file_basename)
              b.build_stats_running_rules))
    in
    Printf.eprintf
      "\027[1m[%4d/%4d] \027[34m%s\027[m %s%!"
      b.build_stats_executed
      b.build_stats_to_execute
      (String.make npar '*')
      current_rules;
    Printf.eprintf "\r\027[K"
  end

let begin_command b proc =
  let r = proc.proc_rule in
  let cmd = match proc.proc_last with
      None -> assert false
    | Some cmd -> cmd
  in
  let cmd_args =
    (BuildEngineRules.command_of_command cmd) @ List.map (BuildEngineRules.argument_of_argument r) cmd.cmd_args
  in
  if verbose 1 then print_stat_line b proc else
  if verbose 2 then begin
    Printf.eprintf "[%d.%d] BEGIN '%s' %s\n%!" r.rule_id proc.proc_step
      (term_escape (String.concat "' '" cmd_args))
    (match cmd.cmd_stdout_pipe with
      None -> ""
      | Some filename -> Printf.sprintf "> '%s'" filename);
  end

let errors = ref []

let print_file message filename =
  let ic = open_in filename in
  let message_printed = ref false in
  begin
    try
      while true do
	let line = input_line ic in
	if not !message_printed then begin
	  message_printed := true;
	  Printf.eprintf "%s\n" message
	end;
	Printf.eprintf "%s\n" line
      done
    with _ -> ()
  end;
  Printf.eprintf "%!";
  close_in ic

let temp_stdout b r =
  Filename.concat b.build_dir_filename
    (Printf.sprintf "rule_%d.stdout" r.rule_id)

let temp_stderr b r =
  Filename.concat b.build_dir_filename
    (Printf.sprintf "rule_%d.stderr" r.rule_id)

let end_command b proc time status =
  match proc.proc_last with
  | None -> assert false
  | Some cmd ->
    let r = proc.proc_rule in
    let cmd_args =
      (BuildEngineRules.command_of_command cmd)
      @ List.map (BuildEngineRules.argument_of_argument r) cmd.cmd_args
    in
    let has_stderr = (Unix.stat (temp_stderr b r)).Unix.st_size > 0 in
    begin
      if verbose 2 then begin
        Printf.eprintf "[%d.%d]   END(%d) '%s'\n%!" r.rule_id proc.proc_step
          status
          (term_escape (String.concat "' '" cmd_args));
      end
      else
        if verbose 1 then
          if !color then
            Printf.eprintf "\027[36m%2.2fs\027[m %s %s\027[m\n"
              time (pretty_rule_name r (!columns - 16))
              (if status = 0 then
                 if has_stderr then "\027[33m[ done ]" else "\027[32m[ done ]"
               else "\027[31m[failed]")
          else
            let percent = b.build_stats_executed * 100 / b.build_stats_to_execute
            in
            Printf.eprintf "[%3d%%] %-50s %s\n%!" percent
              r.rule_main_target.file_basename
              (if status = 0 then "   OK" else "ERROR")
        else
          let point = b.build_stats_executed * 70 / b.build_stats_to_execute
          in
          if point > b.build_stats_lastpoint then begin
            Printf.eprintf ".%!";
            b.build_stats_lastpoint <- point
          end;
    end;
    let str_command =
      term_escape
        (Filename.basename
           (String.concat " " (BuildEngineRules.command_of_command cmd)))
    in
    let color_begin, color_end =
      if not !color then "", ""
      else if status = 0 then "\027[33m", "\027[m"
      else "\027[31m", "\027[m"
    in
    if cmd.cmd_stdout_pipe = None then
      print_file
        (Printf.sprintf "%s-- stdout of %s --%s" color_begin str_command color_end)
        (temp_stdout b r);
    if has_stderr then
      print_file
        (Printf.sprintf "%s-- stderr of %s --%s" color_begin str_command color_end)
        (temp_stderr b r);
    if !color then print_stat_line b proc;
    if status <> 0 then
      errors :=
	[
	  Printf.sprintf "[%d.%d] '%s'" r.rule_id proc.proc_step
	    (term_escape (String.concat "' '" cmd_args));
	  File.string_of_file (temp_stdout b r);
	  File.string_of_file (temp_stderr b r);
	] :: !errors


let add_error s =  errors := s :: !errors
let has_error () = !errors <> []
let errors () = List.rev !errors
let finish () =
  if verbose 0 && not (verbose 1) then
    Printf.eprintf "Finished\n%!"
