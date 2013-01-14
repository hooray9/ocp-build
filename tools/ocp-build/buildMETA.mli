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

 (* comma or space separated *)

module TYPES : sig

type meta = {
  mutable meta_version : string option;
  mutable meta_description : string option;
  mutable meta_requires : (string * string list ref) list;
  mutable meta_archive : (string * string list ref) list;
  mutable meta_exists_if : string option;
  mutable meta_package : (string * meta) list;
}

end

val create_meta_file : string -> TYPES.meta -> unit
val add_requires : TYPES.meta -> string list -> string list -> unit
val add_archive : TYPES.meta -> string list -> string list -> unit
