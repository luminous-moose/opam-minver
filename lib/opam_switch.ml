open Result_syntax

let prefix = "opam-minver-"

let create ~name ~compiler =
  Opam_run.run_opam
    [
      "switch";
      "create";
      name;
      "ocaml-base-compiler." ^ compiler;
      "--yes";
      "--no-switch";
    ]

let remove ~name = Opam_run.run_opam [ "switch"; "remove"; name; "--yes" ]

let list_ours () =
  let* r =
    Process.run "opam" [ "switch"; "list"; "--short"; "--color=never" ]
  in
  if r.Process.exit_code <> 0 then Error r.Process.stderr
  else
    Ok
      (r.Process.stdout |> String.split_on_char '\n'
      |> List.filter (String.starts_with ~prefix))

let find_or_create ~name ~compiler =
  let* existing = list_ours () in
  if List.mem name existing then Ok () else create ~name ~compiler

let current_ocaml_version () =
  let* r = Process.run "ocaml" [ "-vnum" ] in
  if r.Process.exit_code <> 0 then Error r.Process.stderr
  else Ok (Version.of_string (String.trim r.Process.stdout))
