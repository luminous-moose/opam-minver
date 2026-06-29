type build_error = Missing_libraries of string list | Build_failure of string

let extract_missing_library line =
  let prefix = {|Error: Library "|} in
  let suffix = {|" not found|} in
  if String.starts_with ~prefix line then
    let rest = String.sub line (String.length prefix) (String.length line - String.length prefix) in
    match String.index_opt rest '"' with
    | None -> None
    | Some i ->
        let after = String.sub rest i (String.length rest - i) in
        if String.starts_with ~prefix:suffix after then Some (String.sub rest 0 i)
        else None
  else None

module StringSet = Set.Make (String)

let classify_error stderr =
  let lines =
    String.split_on_char '\n' stderr
    |> List.map String.trim
    |> List.filter (fun s -> s <> "")
  in
  let error_lines = List.filter (String.starts_with ~prefix:"Error:") lines in
  match error_lines with
  | [] -> Build_failure stderr
  | _ ->
      let missing = List.filter_map extract_missing_library error_lines in
      if List.length missing = List.length error_lines then
        let missing = StringSet.of_list missing |> StringSet.elements in
        Missing_libraries missing
      else Build_failure stderr

let run_dune ~switch ~dir args =
  let profile_args = [ "--profile=release" ] in
  match
    Process.run ~cwd:dir "opam"
      ([ "exec"; "--switch"; switch; "--"; "dune" ] @ args @ profile_args)
  with
  | Error msg -> Error (Build_failure msg)
  | Ok r ->
      if r.Process.exit_code = 0 then Ok ()
      else Error (classify_error r.Process.stderr)

let build ~switch ~dir ~package =
  run_dune ~switch ~dir [ "build"; "-p"; package ]

let test ~switch ~dir ~package =
  run_dune ~switch ~dir [ "runtest"; "-p"; package ]
