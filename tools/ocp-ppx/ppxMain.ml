
let anon_args = ref []

let arg_list = [

]

let arg_anon x = anon_args := x :: !anon_args
let arg_usage =
  Printf.sprintf "%s [OPTIONS] PLUGINS IN_FILE OUT_FILE"
    (Filename.basename Sys.argv.(0))

let write_ast filename magic input_name ast =
  let oc = open_out_bin filename in
  output_string oc magic;
  output_value oc input_name;
  output_value oc ast;
  close_out oc

let load_apply_save ic magic rewriters out_file =
  let input_name = (input_value ic : string) in
  let ast = (input_value ic : Parsetree.structure) in
  close_in ic;

  let ast = PpxPlugin.apply_all PpxPlugin.structure_rewriters ast in

  write_ast out_file Config.ast_impl_magic_number input_name ast

let _ =
  Arg.parse arg_list arg_anon arg_usage;

  match !anon_args with
  | out_file :: in_file :: plugins ->

    List.iter (fun plugin ->
      try
        Dynlink.loadfile plugin
      with Dynlink.Error error ->
        Printf.eprintf "Error while loading plugin %S\n" plugin;
        Printf.eprintf "Dynlink error %s\n%!" (Dynlink.error_message error);
        exit 2
    ) (List.rev plugins);

    let ic = open_in_bin in_file in
    let buffer = Misc.input_bytes ic
      (String.length Config.ast_impl_magic_number) in
    if buffer = Config.ast_impl_magic_number then
      load_apply_save ic Config.ast_impl_magic_number
        PpxPlugin.structure_rewriters out_file
    else
      if buffer = Config.ast_intf_magic_number then
        load_apply_save ic Config.ast_intf_magic_number
          PpxPlugin.signature_rewriters out_file
      else
        begin
          close_in ic;
          Printf.eprintf "Error: bad magic number %S\n" buffer;
          exit 2
        end
  | _ ->
    Printf.eprintf "Error: you must specify both an IN_FILE and an OUT_FILE\n";
    Arg.usage arg_list arg_usage;
    exit 2
