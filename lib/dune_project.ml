let is_generate_opam_files sexp =
  let open Sexplib0.Sexp in
  match sexp with
  | List (Atom "generate_opam_files" :: rest) -> (
      match rest with [] | [ Atom "true" ] -> true | _ -> false)
  | _ -> false

let generates_opam_files ~dir =
  let path = Filename.concat dir "dune-project" in
  if not (Sys.file_exists path) then Ok false
  else
    let content = In_channel.with_open_text path In_channel.input_all in
    match Parsexp.Many.parse_string content with
    | Error e ->
        let pos = Parsexp.Parse_error.position e in
        Error
          (Printf.sprintf "failed to parse %s:%d:%d: %s" path pos.line pos.col
             (Parsexp.Parse_error.message e))
    | Ok sexps -> Ok (List.exists is_generate_opam_files sexps)
