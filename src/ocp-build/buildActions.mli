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

(* open BuildBase *)

(* clean all generated object files *)
val delete_file_or_directory : string -> unit
val time_step : string -> unit
val time_steps : (string * float) list ref

type project_info = {
  project_dir : File.t;
  cin : BuildOptions.config_input;
  install_where : BuildOCamlInstall.install_where;
  cout : BuildOCamlConfig.TYPES.config_output;
  cfg : BuildOCamlConfig.TYPES.ocaml_config;
}

val load_project : unit -> project_info
