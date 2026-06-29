open Result_syntax

type version_type = Contains_base | Versions of Version.t list

let tokenize s =
  s |> String.split_on_char '\n'
  |> List.concat_map (String.split_on_char ' ')
  |> List.filter (fun s -> s <> "")

let available ~package =
  let* r =
    Process.run "opam"
      [ "show"; package; "--field=all-versions"; "--color=never" ]
  in
  if r.Process.exit_code <> 0 then Error r.Process.stderr
  else
    let tokens = tokenize r.Process.stdout in
    if List.mem "base" tokens then Ok Contains_base
    else
      let versions = List.map Version.of_string tokens in
      Ok (Versions (Version.sort_and_filter versions))

let available_compilers () =
  let* result = available ~package:"ocaml-base-compiler" in
  match result with Versions l -> Ok l | Contains_base -> assert false
