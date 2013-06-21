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

val init : unit -> unit
val begin_command :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_process -> unit
val end_command :
  BuildEngineTypes.build_context ->
  BuildEngineTypes.build_process ->
  float ->
  int ->
  unit
val print_file : string -> string -> unit
val add_error : string list -> unit
val has_error : unit -> bool
val errors : unit -> string list list
val finish : unit -> unit
