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

(*
peerocaml:~/.opam/4.00.1/lib/ocaml%  ocamlfind printconf
Effective configuration:
Configuration file:
    /home/lefessan/.opam/4.00.1/lib/findlib.conf
Search path:
    /home/lefessan/.opam/4.00.1/lib
Packages will be installed in/removed from:
    /home/lefessan/.opam/4.00.1/lib
META files will be installed in/removed from:
    the corresponding package directories
The standard library is assumed to reside in:
    /home/lefessan/.opam/4.00.1/lib/ocaml
The ld.conf file can be found here:
    /home/lefessan/.opam/4.00.1/lib/ocaml/ld.conf
*)

let load_config () =
  let (status, lines) =
    BuildConfig.get_stdout_lines
      [ "ocamlfind" ] [ "printconf" ]
  in
  let search_path = ref None in
  let rec iter lines =
    match lines with
      "Search path:" :: path :: lines when String.length path > 4 ->
        search_path := Some (String.sub path 4 (String.length path - 4));
        iter lines
    | [] -> ()
    | _ :: lines -> iter lines
  in
  iter lines;
  !search_path
