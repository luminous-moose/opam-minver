open Result_syntax

let add ~switch ~package ~version =
  Opam_run.run_opam
    [ "pin"; "add"; "--switch"; switch; "--yes"; package; version ]

let remove ~switch ~package =
  let* () =
    Opam_run.run_opam [ "pin"; "remove"; "--switch"; switch; "--yes"; package ]
  in
  Opam_run.run_opam [ "upgrade"; "--switch"; switch; "--yes" ]

let remove_all ~switch =
  match
    Process.run "opam"
      [ "pin"; "list"; "--switch"; switch; "--short"; "--color=never" ]
  with
  | Error _ -> Ok ()
  | Ok r when r.Process.exit_code <> 0 -> Ok ()
  | Ok r ->
      let packages =
        r.Process.stdout |> String.split_on_char '\n'
        |> List.filter_map (fun s ->
            let s = String.trim s in
            if s = "" then None else Some s)
      in
      if packages = [] then Ok ()
      else
        let* () =
          Opam_run.run_opam
            ([ "pin"; "remove"; "--switch"; switch; "--yes" ] @ packages)
        in
        Opam_run.run_opam [ "upgrade"; "--switch"; switch; "--yes" ]
