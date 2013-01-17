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

type install_where = {
  install_libdirs : string list;
  install_bindir : string;
  install_datadir : string option;

  install_ocamlfind : string list;
  install_ocamllib : string;
}

type install_what = {
  install_byte_bin : bool;
  install_asm_bin : bool;
  install_byte_lib : bool;
  install_asm_lib : bool;
}

val install : bool (* true = install, false = uninstall *) ->
  install_where -> install_what ->
  BuildTypes.package_info -> unit

val uninstall_by_name : install_where ->  string -> unit
val uninstall : install_where -> BuildTypes.package_info -> unit
