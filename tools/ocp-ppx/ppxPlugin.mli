type 'a rewriters

val structure_rewriters : Parsetree.structure rewriters
val signature_rewriters : Parsetree.signature rewriters

val register : 'a rewriters -> string -> ('a -> 'a) -> unit
val apply_all : 'a rewriters -> 'a -> 'a

