
type 'a rewriters = (string * ('a -> 'a) ) list ref

let structure_rewriters = (ref [] : Parsetree.structure rewriters)
let signature_rewriters = (ref [] : Parsetree.signature rewriters)

let register plugins name transform =
  plugins := (name, transform) :: !plugins

let apply_all plugins ast =
  let plugins = List.rev !plugins in
  List.fold_left (fun ast (name, transform) ->
    try
      transform ast
    with e ->
      Printf.eprintf "Error with parsetree transform %S\n%!" name;
      Printf.eprintf "Exception %s\n%!" (Printexc.to_string e);
      exit 2
  ) ast plugins
