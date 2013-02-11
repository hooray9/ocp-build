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

open BuildOCPTypes
open BuildOCPVariable



(* Misc *)
val get_stdout_lines : string list -> string list -> int * string list
val number_of_cores : unit -> int
val split_version : string -> string * string * string
val find_in_PATH : string -> string list -> string
val get_PATH : unit -> string list
