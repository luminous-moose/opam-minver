open Printf
open Opam_minver
open Result_syntax

let () = Printexc.record_backtrace true

let datestamped_log_name () =
  let now = Unix.localtime (Unix.time ()) in
  sprintf "%04d%02d%02d-%02d%02d%02d-opam-minver.log" (now.tm_year + 1900)
    (now.tm_mon + 1) now.tm_mday now.tm_hour now.tm_min now.tm_sec

let setup_logging = function
  | None -> ()
  | Some path ->
      let fname = if path = "" then datestamped_log_name () else path in
      let ch = open_out fname in
      Logs.set_reporter
        (Logs_fmt.reporter ~dst:(Format.formatter_of_out_channel ch) ());
      Logs.set_level (Some Logs.Debug)

let find_opam_file dir =
  match
    Listdir.pathlistdir dir
    |> List.filter (fun f -> Filename.check_suffix f ".opam")
  with
  | [] -> Error (sprintf "no .opam file found in %s" dir)
  | [ f ] -> Ok f
  | _ -> Error (sprintf "multiple .opam files found in %s" dir)

let delete_switches ~dry_run =
  let* switches = Opam_switch.list_ours () in
  match switches with
  | [] ->
      print_endline "No switches to delete.";
      Ok ()
  | switches ->
      List.iter
        (fun name ->
          if dry_run then printf "would delete switch %s\n%!" name
          else begin
            printf "deleting switch %s\n%!" name;
            match Opam_switch.remove ~name with
            | Error msg ->
                eprintf "opam-minver: failed to remove %s: %s\n" name msg
            | Ok () -> ()
          end)
        switches;
      Ok ()

let cmd_run dir write_out keep_switches keep_json log_file quiet =
  let verbose = not quiet in
  setup_logging log_file;
  let generates_opam_files =
    match Dune_project.generates_opam_files ~dir with
    | Error msg ->
        if write_out then
          eprintf "opam-minver: warning: %s\nDisabling write-out.\n" msg;
        false
    | Ok v -> v
  in
  let preamble =
    if generates_opam_files then
      Some
        "opam-minver: warning: this project uses dune's generate_opam_files; \
         the .opam file is managed by dune and would be overwritten on the \
         next build. Discovered bounds will be printed but not written."
    else None
  in
  let write_out = write_out && not generates_opam_files in
  match find_opam_file dir with
  | Error msg ->
      eprintf "opam-minver: %s\n" msg;
      exit 1
  | Ok opam_path -> (
      let manifest =
        match Manifest_parse.read opam_path with
        | Error msg ->
            eprintf "opam-minver: %s\n" msg;
            exit 1
        | Ok m -> m
      in
      let state = State.load ~dir in
      match Runner.run state dir manifest ~preamble ~verbose ~write_out with
      | Error msg ->
          eprintf "opam-minver: %s\n" msg;
          exit 1
      | Ok () ->
          (if not keep_switches then
             match delete_switches ~dry_run:false with
             | Error msg ->
                 eprintf "opam-minver: warning: could not delete switches: %s\n"
                   msg
             | Ok () -> ());
          if write_out && not keep_json then begin
            let json_path = Filename.concat dir "opam-minver.json" in
            if Sys.file_exists json_path then begin
              Sys.remove json_path;
              printf "Removed state file %s.\n" json_path
            end
          end)

let cmd_delete dry_run =
  match delete_switches ~dry_run with
  | Error msg ->
      eprintf "opam-minver: %s\n" msg;
      exit 1
  | Ok () -> ()

(* ---- Argument definitions ---- *)

open Cmdliner

let s_main = "MAIN OPTIONS"

let dir_opt =
  let doc =
    "Path to the project directory containing the .opam file. Defaults to the \
     current working directory."
  in
  Arg.(value & opt string "." & info [ "dir" ] ~docv:"DIR" ~doc)

let write_flag =
  let doc =
    "Write discovered lower bounds back to the .opam file. Without this flag \
     the tool runs in dry-run mode, printing what it would write without \
     modifying anything."
  in
  Arg.(value & flag & info [ "write"; "w" ] ~docs:s_main ~doc)

let keep_switches_flag =
  let doc =
    "Keep temporary opam switches after a successful run. By default all \
     $(b,opam-minver-) switches are removed on success."
  in
  Arg.(value & flag & info [ "keep-switches" ] ~doc)

let keep_json_flag =
  let doc =
    "Keep the opam-minver.json state file after a successful run with \
     $(b,--write). By default it is deleted once results have been written to \
     the .opam file."
  in
  Arg.(value & flag & info [ "keep-json" ] ~doc)

let log_file_opt =
  let doc =
    "Enable logging. If $(docv) is omitted, a timestamped filename is \
     generated automatically (e.g. 20260616-120000-opam-minver.log). Log files \
     record each probe invocation and its outcome, which is useful for \
     understanding why a particular package version was rejected as \
     incompatible."
  in
  Arg.(
    value
    & opt ~vopt:(Some "") (some string) None
    & info [ "log-file" ] ~docv:"FILE" ~doc)

let quiet_flag =
  let doc = "Suppress per-probe progress output." in
  Arg.(value & flag & info [ "quiet"; "q" ] ~doc)

let dry_run_flag =
  let doc = "Show which switches would be deleted without removing them." in
  Arg.(value & flag & info [ "dry-run"; "n" ] ~doc)

(* ---- Terms ---- *)

let run_term =
  Term.(
    const cmd_run $ dir_opt $ write_flag $ keep_switches_flag $ keep_json_flag
    $ log_file_opt $ quiet_flag)

let delete_term = Term.(const cmd_delete $ dry_run_flag)

(* ---- Command infos ---- *)

let run_info =
  Cmd.info "opam-minver"
    ~doc:"find minimum dependency versions for an opam project" ~version:"0.1.0"
    ~man:
      [
        `S Manpage.s_description;
        `P
          "$(tname) reads a project's .opam file, then binary-searches each \
           dependency's available versions to find the oldest set at which the \
           project successfully builds and passes its tests. The project must \
           build and pass its tests with the currently active compiler before \
           the search begins: if it does not, the run aborts.";
        `P
          "Any existing dependency version bounds in the .opam file are used \
           as limits to the search space. This can save time if you know of \
           incompatibilities, but it also means that if you don't, you should \
           probably remove any bounds you aren't certain of and let the tool \
           discover them.";
        `P
          "By default the tool runs in dry-run mode and prints what it would \
           write. Pass $(b,--write) to update the .opam file in place.";
        `P
          "Temporary opam switches are created during the search and removed \
           on success. Pass $(b,--keep-switches) to retain them for \
           inspection.";
        `P
          "Progress is saved to $(b,opam-minver.json) after every probe so a \
           run can be resumed if interrupted. Pass $(b,--keep-json) to retain \
           this file after the results are written. If you change the \
           project's compatibility requirements between runs, such as dropping \
           OCaml 4 support, raising a lower bound manually, or adding a new \
           dependency, delete $(b,opam-minver.json) before re-running, or \
           cached results from the previous run may prevent the new bounds \
           from being found.";
        `S s_main;
        `S Manpage.s_options;
      ]

let delete_info =
  Cmd.info "delete"
    ~doc:"Delete all temporary opam switches created by $(mname)."
    ~man:
      [
        `S Manpage.s_description;
        `P
          "Removes every opam switch whose name begins with the \
           $(b,opam-minver-) prefix. Use $(b,--dry-run) to preview what would \
           be removed.";
      ]

let () =
  Cmd.group run_info ~default:run_term [ Cmd.v delete_info delete_term ]
  |> Cmd.eval |> exit
