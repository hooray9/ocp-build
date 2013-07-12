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



type env = { env : plist StringMap.t }
and plist = (string * env) list

type 'a source_option = {
  get : env -> 'a;
  set : 'a -> unit;
}

let empty_env = { env = StringMap.empty }
let global_env = ref StringMap.empty
let set_global name v =
  global_env := StringMap.add name v !global_env

let set env name v = { env = StringMap.add name v env.env }
let get_local env name =
  try
    StringMap.find name env.env
  with e ->
(*    Printf.eprintf "get_local %S failed\n%!" name; *)
    raise e

let get env name =
  try get_local env name
  with Not_found ->
    try
      StringMap.find name !global_env
    with e ->
(*      Printf.eprintf "get_local %S failed\n%!" name; *)
      raise e

let true_value =  [ ".", { env = StringMap.empty } ]
let false_value = []

let plist_of_bool b =
  if b then true_value else false_value
let bool_of_plist v =
  match v with
  |  [] -> false
  | _ -> true

let plist_of_strings strings =
   (List.map (fun s -> s, { env = StringMap.empty }) strings)
let strings_of_plist list =
  List.map (fun (s,_) -> s) list

let plist_of_string s = [ s, { env = StringMap.empty } ]
let string_of_plist list = String.concat " " (strings_of_plist list)

let set_bool env name v = set env name (plist_of_bool v)
let get_bool env name = bool_of_plist (get env name)

let set_strings env name v = set env name (plist_of_strings v)
let get_strings env name = strings_of_plist (get env name)

let set_string env name v = set env name (plist_of_string v)
let get_string env name = string_of_plist (get env name)

let get_bool_with_default env name bool =
  try get_bool env name with Not_found -> bool

let get_strings_with_default env name s =
  try get_strings env name with Not_found -> s

let get_string_with_default env name s =
  try get_string env name with Not_found -> s

(*

(* open BuildBase *)
(* open Stdlib2 *)

type options = option_value StringMap.t

and option_value =
  | OptionList of (string * options) list

let new_options = StringMap.empty

let default_options = new_options

type 'a source_option = {
  option_name : string;
  mutable option_default : 'a;
}

let options = ref []

let new_bool_option name default =
  { option_name = name; option_default = default }

let new_initial_bool_option name default =
  let o = new_bool_option name default in
  options := (fun vars -> StringMap.add name (option_value_of_bool o.option_default) vars)
      :: !options;
  o

let new_strings_option name (default : string list) =
  { option_name = name; option_default = default }

let new_initial_strings_option name default =
  let o = new_strings_option name default in
  options := (fun vars ->
(*    Printf.fprintf stderr "Setting as default %s = %S\n%!" name
      (String.concat ";" o.option_default); *)
    StringMap.add name (option_value_of_strings o.option_default) vars)
      :: !options;
  o

let options_find option options =
(*  try *)
    StringMap.find option.option_name options
(*  with Not_found as e ->
    match options.options_inherit with
	None -> raise e
      | Some options ->
	StringMap.find option.option_name options.options_vars
*)

let bool_option_true options bool_option =
  try
    bool_of_option_value ( options_find bool_option options )
  with Not_found -> bool_option.option_default

let set_strings_option strings_option default =
  strings_option.option_default <- default

let strings_option options strings_option =
  try
    strings_of_option_value ( options_find strings_option options )
  with Not_found -> strings_option.option_default

let string_option options string_option =
  String.concat " " (strings_option options string_option)

let get_strings_option strings_option =
  String.concat " " strings_option.option_default


let string_option_with_default options name default =
  try
    String.concat " "  (strings_of_option_value ( StringMap.find name options ))
  with Not_found -> default

let list_option_with_default options name default =
  try
    strings_of_option_value ( StringMap.find name options )
  with Not_found -> default

let bool_option_with_default options name default =
  try
    bool_of_option_value ( StringMap.find name options )
  with Not_found -> default

(* let enabled_option = new_bool_option "enabled" true *)
*)

let is_already_installed options =
  get_bool_with_default options "generated" false
  || get_bool_with_default options "installed" false

let new_bool_option name v =
  set_global name (plist_of_bool v);
  {
    get = (fun env -> get_bool env name);
    set = (fun v -> set_global name (plist_of_bool v));
  }

let new_strings_option name v =
  set_global name (plist_of_strings v);
  {
    get = (fun env -> get_strings env name);
    set = (fun v -> set_global name (plist_of_strings v));
  }

let new_string_option name v =
  set_global name (plist_of_string v);
  {
    get = (fun env -> get_string env name);
    set = (fun v -> set_global name (plist_of_string v));
  }
