
let cwd = Unix.getcwd ()

let descr_arg = ref None
let opam_arg = ref None
let login_arg = ref None
let targets_arg = ref []

let arg_list = [
  "-login", Arg.String (fun s -> login_arg := Some s ),
  " <login> : github login";

  "-opam", Arg.String (fun s -> opam_arg := Some s ),
  " <opam> : the package/opam file for this version";

  "-descr", Arg.String (fun s -> descr_arg := Some s ),
  " <descr> : the package/descr file for this version";

]
let arg_anon s = targets_arg := s :: !targets_arg
let arg_usage = "ocp-opamer [OPTIONS] package_name version tarball_url"

let _ =
  Arg.parse arg_list arg_anon arg_usage

let (package, version, url) =
  match !targets_arg with
    [ url; version; package ] -> (package, version, url)
  | _ -> Arg.usage arg_list arg_usage; exit 2

let home_dir = Sys.getenv "HOME"
let opam_dir = Filename.concat home_dir ".opam"

let opamer_dir = Filename.concat opam_dir "opamer"
let _ = File.RawIO.safe_mkdir opamer_dir

let login =
  let login_file = Filename.concat opamer_dir "login.txt" in
  match !login_arg with
  | None -> begin
    if not (Sys.file_exists login_file) then
      failwith "You must provide your GitHub login";
    match File.lines_of_file login_file with
      [] -> failwith (Printf.sprintf "Empty file %S" login_file)
    | login :: _ -> login
  end
  | Some login ->
    if not (Sys.file_exists login_file) then
      File.file_of_lines login_file [ login ];
    login

let repository_dir =
  Filename.concat opamer_dir "opam-repository"

let _ =
  Unix.chdir opamer_dir

let package_dir = Filename.concat opamer_dir
  (Printf.sprintf "packages/%s" package)
let _ =
  File.RawIO.safe_mkdir package_dir

let descr_file = Filename.concat package_dir "descr"
let opam_file = Filename.concat package_dir "opam"
let archive_file = Filename.concat package_dir "archive"

let descr = match !descr_arg with
    None ->
      if not (Sys.file_exists descr_file) then
        failwith "You must provide a descr file as argument";
      File.string_of_file descr_file
  | Some descr_file ->
    let descr_file = if Filename.is_relative descr_file then
        Filename.concat cwd descr_file
      else descr_file in
    let descr = File.string_of_file descr_file in
    File.file_of_string descr_file descr;
    descr

let opam = match !opam_arg with
    None ->
      if not (Sys.file_exists opam_file) then
        failwith "You must provide a opam file as argument";
      File.string_of_file opam_file
  | Some opam_file ->
    let opam_file = if Filename.is_relative opam_file then
        Filename.concat cwd opam_file
      else opam_file in
    let opam = File.string_of_file opam_file in
    File.file_of_string opam_file opam;
    opam

let md5sum =
  let cmd = Printf.sprintf
    "wget --passive-ftp -q -O '%s' '%s'" archive_file
    url in
  if Sys.command cmd <> 0 then begin
    failwith "could not download archive"
  end;
  let md5sum = Digest.file archive_file in
  Digest.to_hex md5sum

let _ =
(* first, create a clone of the opam-repository repository *)
  if not (Sys.file_exists repository_dir) then begin
    let cmd = Printf.sprintf
      "git clone git@github.com:lefessan/opam-repository.git opam-repository"
    in
    if Sys.command cmd <> 0 then begin
      Printf.eprintf "Error: cloning your GitHub fork failed:\n";
      Printf.eprintf "cmd: %S\n" cmd;
      Printf.eprintf "You must fork OCamlPro/opam-repository on your GitHub account!\n%!";
      exit 2
    end;

    Unix.chdir repository_dir;
    let cmd =
      "git remote add ocamlpro git@github.com:OCamlPro/opam-repository.git"
    in
    assert (Sys.command cmd = 0)
  end
  else begin
    Unix.chdir repository_dir;
  end;

(* fetch OCamlPro/opam-repository *)
  let cmd =
    "git fetch ocamlpro"
  in
  if Sys.command cmd <> 0 then begin
    Printf.eprintf "Error: could not fetch latest version of OCamlPro's\n";
    Printf.eprintf "repository.\n";
    Printf.eprintf "No connection ?\n%!";
    exit 2
  end;

(* fetch your opam-repository *)
  let cmd =
    "git fetch origin"
  in
  if Sys.command cmd <> 0 then begin
    Printf.eprintf "Error: could not fetch latest version of your's\n";
    Printf.eprintf "repository.\n";
    Printf.eprintf "No connection ?\n%!";
    exit 2
  end;


(* for this package, checkout the correct branch *)
  let cmd = Printf.sprintf "git checkout %s" package
  in
  if Sys.command cmd <> 0 then begin

    let cmd = "git checkout master" in
    if Sys.command cmd <> 0 then begin
      Printf.eprintf "Error: could not checkout the master branch.\n";
      Printf.eprintf "Do you have uncommited changes ?\n";
      exit 2
    end;

    let cmd = Printf.sprintf "git checkout -b %s" package in
    if Sys.command cmd <> 0 then begin
      Printf.eprintf "Error: could not create a new branch %S.\n" package;
      Printf.eprintf "Maybe you already have one, with  uncommited changes ?\n";
      exit 2
    end;

  end;

  (* Synchronize with OCamlPro/master *)
  let cmd = "git merge ocamlpro/master" in
  if Sys.command cmd <> 0 then begin
      Printf.eprintf "Error: could not merge with the master branch.\n";
      Printf.eprintf "Do you have uncommited changes ?\n";
      exit 2
  end;

  let local_dir = Printf.sprintf "packages/%s.%s" package version in

  let package_version_dir =
    Filename.concat repository_dir local_dir in
  Printf.eprintf "package_version_dir: %s\n%!" package_version_dir;
  File.RawIO.safe_mkdir package_version_dir;
  let package_opam_file = Filename.concat package_version_dir "opam" in
  let package_descr_file = Filename.concat package_version_dir "descr" in
  let package_url_file = Filename.concat package_version_dir "url" in
  File.file_of_string package_opam_file opam;
  File.file_of_string package_descr_file descr;
  File.file_of_string package_url_file
    (Printf.sprintf "url: %s\nmd5sum: %s\n"
       url md5sum);

  Unix.chdir local_dir;

  let cmd = Printf.sprintf
    "git add opam descr url"
  in
  if Sys.command cmd <> 0 then begin
    Printf.eprintf "Error: git could not add files ?\n";
    Printf.eprintf "Weird...\n%!";
    exit 2
  end;

  let cmd = Printf.sprintf
    "git commit -m '%s version %s' ." package version
  in
  if Sys.command cmd <> 0 then begin
    Printf.eprintf "Error: git could not add files ?\n";
    Printf.eprintf "Weird...\n%!";
    exit 2
  end;

(* push to your opam-repository *)
  let cmd = Printf.sprintf
    "git push origin %s" package
  in
  if Sys.command cmd <> 0 then begin
    Printf.eprintf "Error: could not fetch latest version of your's\n";
    Printf.eprintf "repository.\n";
    Printf.eprintf "No connection ?\n%!";
    exit 2
  end;



