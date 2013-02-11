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

(* Génération et lecture du fichier de configuration *)

let verbose = DebugVerbosity.verbose [ "B" ] "BuildConfig"

open OcpLang

(*
open BuildOptions
open Genlex
open BuildOCPTypes
open BuildGlobals
open BuildTypes
open BuildMisc
*)


let sep_PATH =
  match Sys.os_type with
      "Win32" -> ';'
    | _ -> ':'

let get_PATH () =
  try
    let path = Sys.getenv "PATH" in
    String.split path sep_PATH
  with Not_found ->
    failwith "Env variable PATH not defined"

let b = Buffer.create 10000
let get_stdout_lines cmd args =
  let temp_file = Filename.temp_file "ocp-build-" ".out" in
  let pid = BuildMisc.create_process (cmd@args) (Some temp_file) None in
  let status = BuildMisc.wait_command pid in
  let lines = ref [] in
  begin try
	  let ic = open_in temp_file in
	  begin

	    try
	      while true do
		lines := (input_line ic) :: !lines
	      done
	    with _ -> ()
	  end;
	  close_in ic;
	  Sys.remove temp_file;
    with _ -> ()
  end;
  (status, List.rev !lines)

let check_command_exists filename =
  let st = Unix.stat filename in
  match st.Unix.st_kind with
      Unix.S_REG ->
	begin
	  try
	    Unix.access filename [Unix.X_OK];
	    filename
	  with e ->
	    Printf.eprintf "Warning: %s in PATH has not executable permission\n%!"
	      filename;
	    raise e
	end
    | _ ->
      Printf.eprintf "Warning: %s in PATH is not a regular command\n%!" filename;
      raise Not_found

let rec find_in_PATH command path =
  match path with
      [] -> raise Not_found
    | dirname :: path ->
      let filename = Filename.concat dirname command in
      try
	check_command_exists filename
      with _ ->
	if Win32.os_type = Win32.WINDOWS || Win32.os_type = Win32.CYGWIN then
	  try
	    check_command_exists (filename ^ ".exe")
	  with _ ->
	    find_in_PATH command path
	else
	  find_in_PATH command path

let split_version version =
  let version = String.copy version in
  for i = 0 to String.length version - 1 do
    match version.[i] with
      '0'..'9' -> ()
    | _ -> version.[i] <- ' '
  done;
  match OcpString.split_simplify version ' ' with
  | [ major ] -> (major, "00", "0")
  | [ major; minor ] -> (major, minor, "0")
  | major :: minor :: point :: _ -> (major, minor, point)
  | [] -> failwith "Could not set major/minor/point version"

let number_of_cores () =
  let ncores = ref 0 in
(* Compute number of cores, including hyper-threading, on a linux machine *)
  begin try
  File.iter_lines (fun line ->
    if OcpString.starts_with line "processor" then incr ncores
  ) "/proc/cpuinfo"
    with _ -> ()
  end;
  !ncores
