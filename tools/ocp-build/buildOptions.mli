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

open SimpleConfig

val shortcut_arg :
  string -> string ->
  (string * Arg.spec * string) list ->
  (string * Arg.spec * string)


type config_input = {
  mutable cin_ocamlc_variants : string list;
  mutable cin_ocamlopt_variants : string list;
  mutable cin_ocamldep_variants : string list;
  mutable cin_ocamllex_variants : string list;
  mutable cin_ocamlyacc_variants : string list;
  mutable cin_bytecode : bool;
  mutable cin_native : bool;
  mutable cin_ocamlbin : string option;
  mutable cin_ocamllib : string option;
  mutable cin_use_ocamlfind : bool;
  mutable cin_ocps_in_ocamllib : bool;
  mutable cin_meta_dirnames : string list;
  mutable cin_ocps_dirnames : string list;

  mutable cin_autoscan : bool;
  mutable cin_digest : bool;
  mutable cin_verbosity : int;
  mutable cin_njobs : int;

  mutable cin_install_destdir : string option;
  mutable cin_install_bin : string option;
  mutable cin_install_lib : string option;
  mutable cin_install_doc : string option;
  mutable cin_install_data : string option;
}

val arg_list : unit -> (string * Arg.spec * string) list

val load : File.t option -> config_input
val maybe_save : unit -> unit

val must_save_project : unit -> unit

module ProjectOptions : sig
  val install_bin_option : string option SimpleConfig.config_option
  val install_lib_option : string option SimpleConfig.config_option
  val install_data_option :
    string option SimpleConfig.config_option
  val install_doc_option : string option SimpleConfig.config_option
  val autoscan_option : bool option SimpleConfig.config_option
  val ocamllib_option : string option SimpleConfig.config_option
  val project_ocpbuild_version : string SimpleConfig.config_option
  val project_external_dirs_option :
    string list SimpleConfig.config_option
  val root_files : File.t list SimpleConfig.config_option

end
