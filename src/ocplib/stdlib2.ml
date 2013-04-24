
module OcpString : sig

(** [before s pos] returns the substring before [pos] (the character
    at position [pos] is discarded) *)
val before : string -> int -> string

(** [after s pos] returns the substring after [pos] (the character at
    position [pos] is discarded) *)
val after : string -> int -> string

(** [starts_with s ~prefix] checks whether [s] starts with [prefix] *)
val starts_with : string -> prefix:string -> bool

(** [cut_at ?keep c s] returns the substring of [s] before and after
    the position first occurence of character [c]. If [keep] is set,
    then the character at position [pos] is kept (default is
    [keep=false]). *)
val cut_at : ?keep:bool -> string -> char -> string * string

(** [rcut_at ?keep c s] returns the substring of [s] before and after
    the position last occurence of character [c]. If [keep] is set,
    then the character at position [pos] is kept (default is
    [keep=false]). *)
val rcut_at : ?keep:bool -> string -> char -> string * string

(** [split s c] splits the string [s] on characters [c],
   starting from the left. *)
val split : string -> char -> string list

(** [split s c] splits the string [s] on characters [c],
   starting from the left, removing empty sub strings. *)
val split_simplify : string -> char -> string list

end = struct

let rec strneql s1 s2 len =
  len = 0 || (
    let len = len - 1 in
    s1.[len] = s2.[len] && strneql s1 s2 len)

let starts_with s ~prefix =
  let len1 = String.length s in
  let len2 = String.length prefix in
  len2 <= len1 && strneql s prefix len2

(* The substring before [pos] (the character at position [pos] is discarded) *)
let before s pos =
  String.sub s 0 pos

(* The substring after [pos] (the character at position [pos] is discarded) *)
let after s pos =
  String.sub s (pos + 1) (String.length s - pos - 1)

(* Cut the string at position [pos] (the character at position [pos]
   is kept if [keep]) *)
let cut ?(keep=false) s pos =
  try
    let npos = if keep then pos - 1 else pos in
    before s pos, after s npos
  with _ -> s, ""

let cut_at ?(keep=false) s c =
  try
    let pos = String.index s c in
    cut ~keep s pos
  with _ -> s, ""

let rcut_at ?(keep=false) s c =
  try
    let pos = String.rindex s c in
    cut ~keep s pos
  with _ -> s, ""

let split s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev ("" :: to_rev) else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) ("" :: to_rev) else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let _ =
  assert (split "" 'o' = [""]);
  assert (split "toto" 'o' = ["t"; "t"; ""]);
  assert (split "ototo" 'o' = [""; "t"; "t"; ""]);
  assert (split "tot" 'o' = ["t"; "t"]);
  ()

let split_simplify s c =
  let len = String.length s in
  let rec iter pos to_rev =
    if pos = len then List.rev to_rev else
      match try
              Some ( String.index_from s pos c )
        with Not_found -> None
      with
          Some pos2 ->
            if pos2 = pos then iter (pos+1) to_rev else
              iter (pos2+1) ((String.sub s pos (pos2-pos)) :: to_rev)
        | None -> List.rev ( String.sub s pos (len-pos) :: to_rev )
  in
  iter 0 []

let _ =
  assert (split_simplify "" 'o' = []);
  assert (split_simplify "toto" 'o' = ["t"; "t"]);
  assert (split_simplify "ototo" 'o' = ["t"; "t"]);
  assert (split_simplify "tot" 'o' = ["t"; "t"]);
  ()

end


module OcpGenlex : sig

(** Exception ParseError of char_position * message *)
exception ParseError of int * string

(** [tokens_of_string lexer string] returns the list of tokens from
    the string using [lexer]. Raise ParseError in case of error. *)
val tokens_of_string : (char Stream.t -> Genlex.token Stream.t) -> string -> Genlex.token list

end = struct

  open Genlex

exception ParseError of int * string

let tokens_of_string lexer s =
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  try
    Stream.iter (fun token ->
      list := token :: !list) str2;
    List.rev !list
  with
      Stream.Error error ->
	raise (ParseError (Stream.count str1, error))

let tokenlocs_of_string lexer s =
  let str1 = Stream.of_string s in
  let str2 = lexer str1 in
  let list = ref [] in
  try
    Stream.iter (fun token ->
      let token_pos = Stream.count str1 in
      list := (token, token_pos) :: !list) str2;
    List.rev !list
  with
      Stream.Error error ->
	raise (ParseError (Stream.count str1, error))

end

module OcpDigest : sig

(** Return the digest corresponding to the printable hexadecimal representation. *)
val of_hex : string -> Digest.t

end = struct

let of_hex_char c =
  let x = int_of_char c in
  match c with
      'a' .. 'z' -> x - 97 + 10
    | 'A' .. 'Z' -> x - 65 + 10
    | '0' .. '9' -> x - 48
    | _ -> invalid_arg "of_hex_char"

let of_hex d =
  let result = String.create 16 in
  let rec iter result d i i2 =
    let c1 = d.[i2] in
    let c2 = d.[i2+1] in
    let c1 = of_hex_char c1 in
    let c2 = of_hex_char c2 in
    result.[i] <- char_of_int ((c1 lsl 4) + c2);
    if i < 15 then
      iter result d (i+1) (i2+2)
  in
  iter result d 0 0;
  result

end

module StringSet = Set.Make(String)
module StringMap = struct
  module M = Map.Make(String)
  include M


let to_list map =
  let list = ref [] in
  iter (fun x y -> list := (x,y) :: !list) map;
  List.rev !list

end

module Int = struct
  type t = int
  let compare (x:int) y = compare x y
end

module IntSet = Set.Make(Int)

module IntMap = struct

  module M = Map.Make(Int)
  include M


  exception MinElt
  let exn_MinElt = MinElt
  let min_elt map =
    let x = ref None in
    try
      iter (fun key v -> x := Some (key, v); raise exn_MinElt) map;
      None
    with MinElt -> !x

end
