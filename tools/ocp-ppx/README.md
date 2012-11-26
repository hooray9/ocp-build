# ocp-ppx

A simple program to load ppx transformations and apply them on
the parsetree thanks to the -ppx option.

## Usage

The command line arguments of ocp-ppx are:

   ocp-ppx [OPTIONS] PLUGINS in_file out_file

The two last arguments, in_file and out_file, are automatically added
by the compiler when ocp-ppx is called by the compiler for the -ppx
option:

ocamlc -c -ppx 'ocp-ppx OPTIONS PLUGINS' foo.ml

PLUGINS is a list of object files (.cmo or .cma) that register
transformations on parsetrees, either interfaces or implementations.

The skeleton of such plugins uses the 'PpxPlugin' module interface to register
transformations on Parsetree types:

   let interface_transform (intf_ast : Parsetree.interface) = 
      let new_intf_ast = ... intf_ast in
      (new_intf_ast : Parsetree.interface)

   let implementation_transform (impl_ast : Parsetree.implementation) = 
      let new_impl_ast = ... impl_ast in
      (new_impl_ast : Parsetree.implementation)

   let _ =
     let module PPX = PpxPlugin in
     PPX.register PPX.interface_rewriters "MyPlugin" interface_transform;
     PPX.register PPX.implementation_rewriters "MyPlugin" implementation_transform;
     ()


