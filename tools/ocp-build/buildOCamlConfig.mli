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


(* These values are modified by reading the configuration *)
val ocamlc_cmd : string list source_option
val ocamlcc_cmd : string list source_option
val ocamllex_cmd : string list source_option
val ocamlyacc_cmd : string list source_option
val ocamldep_cmd : string list source_option
val ocamlopt_cmd : string list source_option

(*
val mklib_cmd : BuildTypes.mklib_kind ref
val ar_cmd : string ref
val ranlib_cmd : string ref
val libdirs : (string * string) list ref
*)

(* These values are global, but could be set per project, as we can
  change the compiler depending on that.

   TODO: Maybe we could even attach
  these values to a particular compiler, and cache them so that we
   can load them each time that compiler is used.. *)
val ocaml_config_version : string list source_option
val ocaml_config_system : string list source_option
val ocaml_config_architecture :  string list source_option
val ocaml_config_ext_obj :  string list source_option
val ocaml_config_ext_lib :  string list source_option
val ocaml_config_ext_dll :  string list source_option



module TYPES : sig
  type ocaml_config = {
    ocaml_version : string;
    ocaml_version_major : string;
    ocaml_version_minor : string;
    ocaml_version_point : string;
    ocaml_ocamllib : string;
    ocaml_system : string;
    ocaml_architecture : string;
    ocaml_ext_obj : string;
    ocaml_ext_lib : string;
    ocaml_ext_dll : string;
    ocaml_os_type : string;
    ocaml_bin : string;
  }

  type config_input = {
    mutable cin_ocamlc : string list;
    mutable cin_ocamlopt : string list;
    mutable cin_ocamldep : string list;
    mutable cin_ocamllex : string list;
    mutable cin_ocamlyacc : string list;
    mutable cin_bytecode : bool;
    mutable cin_native : bool;
    mutable cin_ocamlbin : string option;
    mutable cin_ocamllib : string option;
    mutable cin_use_ocamlfind : bool;
    mutable cin_ocps_in_ocamllib : bool;
    mutable cin_meta_dirnames : string list;
    mutable cin_ocps_dirnames : string list;
  }

  type config_output = {
    mutable cout_ocaml : ocaml_config option;
    mutable cout_ocamlc : string list option;
    mutable cout_ocamlcc : string list option;
    mutable cout_ocamlopt : string list option;
    mutable cout_ocamldep : string list option;
    mutable cout_ocamlyacc : string list option;
    mutable cout_ocamllex : string list option;
  }

end

open TYPES

val arg_list : unit -> (string * Arg.spec * string) list
val load_global_config : File.t -> unit

val check_config : unit -> config_input * config_output
val set_global_config : config_output -> unit
