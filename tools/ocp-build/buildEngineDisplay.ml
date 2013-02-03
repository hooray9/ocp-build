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

let init b = ()

let begin_command b proc =
  let r = proc.proc_rule in
  let cmd = match proc.proc_last with
      None -> assert false
    | Some cmd -> cmd
  in
  let cmd_args =
    (BuildEngineRules.command_of_command cmd) @ List.map (BuildEngineRules.argument_of_argument r) cmd.cmd_args
  in
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
	  Printf.eprintf "%s\n%!" message
	end;
	Printf.eprintf "%s\n%!" line
      done
    with _ -> ()
  end;
  close_in ic

let temp_stdout b r =
  Filename.concat b.build_dir_filename
    (Printf.sprintf "rule_%d.stdout" r.rule_id)

let temp_stderr b r =
  Filename.concat b.build_dir_filename
    (Printf.sprintf "rule_%d.stderr" r.rule_id)

let end_command b proc status =
  match proc.proc_last with
  | None -> assert false
  | Some cmd ->
    let r = proc.proc_rule in
    let cmd_args =
      (BuildEngineRules.command_of_command cmd) @ List.map (BuildEngineRules.argument_of_argument r) cmd.cmd_args
    in
    begin
      if verbose 2 then begin
        Printf.eprintf "[%d.%d]   END(%d) '%s'\n%!" r.rule_id proc.proc_step
          status
          (term_escape (String.concat "' '" cmd_args));
      end
      else
        if verbose 1 then
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
    if cmd.cmd_stdout_pipe = None then
      print_file  "Command stdout:" (temp_stdout b r);
    print_file  "Command stderr:" (temp_stderr b r);
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
