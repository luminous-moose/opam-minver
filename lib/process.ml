open Printf
open Result_syntax

type t = { exit_code : int; stdout : string; stderr : string }

let am_test = ref false
let buffer_size = 4096

let run ?cwd cmd args : (t, string) Stdlib.result =
  if !am_test then
    failwith
      (sprintf "Process.run called in test mode: %s %s" cmd
         (String.concat " " args));
  let args_array = Array.of_list (cmd :: args) in

  (* Create pipes before spawning. ~cloexec:true prevents these fds from
     leaking into any other children spawned later. *)
  let stdout_r, stdout_w = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr_w = Unix.pipe ~cloexec:true () in

  Logs.debug (fun m -> m "run: %s %s" cmd (String.concat " " args));
  let saved_cwd =
    match cwd with Some _ -> Some (Unix.getcwd ()) | None -> None
  in
  Option.iter Unix.chdir cwd;
  let pid_result =
    match Unix.create_process cmd args_array Unix.stdin stdout_w stderr_w with
    | exception Unix.Unix_error (e, _, _) ->
        List.iter Unix.close [ stdout_r; stdout_w; stderr_r; stderr_w ];
        Error (Unix.error_message e)
    | pid -> Ok pid
  in
  Option.iter Unix.chdir saved_cwd;
  let* pid = pid_result in

  (* Close the write ends in the parent immediately after spawning.
     If the parent holds a write end open, reading the corresponding read end
     will never return EOF, and the select loop will hang forever. *)
  Unix.close stdout_w;
  Unix.close stderr_w;

  let stdout_buf = Buffer.create buffer_size in
  let stderr_buf = Buffer.create buffer_size in

  let bufs = [ (stdout_r, stdout_buf); (stderr_r, stderr_buf) ] in

  let open_fds = ref [ stdout_r; stderr_r ] in
  let tmp = Bytes.create buffer_size in

  while !open_fds <> [] do
    let ready, _, _ = Unix.select !open_fds [] [] (-1.) in
    List.iter
      (fun fd ->
        let buf = List.assoc fd bufs in
        match Unix.read fd tmp 0 buffer_size with
        | 0 -> begin
            Unix.close fd;
            open_fds := List.filter (fun fd' -> fd <> fd') !open_fds
          end
        | n -> Buffer.add_subbytes buf tmp 0 n
        | exception Unix.Unix_error (Unix.EINTR, _, _) -> ())
      ready
  done;

  match snd @@ Unix.waitpid [] pid with
  | WEXITED exit_code -> begin
      let stdout = Buffer.contents stdout_buf in
      let stderr = Buffer.contents stderr_buf in
      Logs.debug (fun m -> m "exit: %d" exit_code);
      if stdout <> "" then Logs.debug (fun m -> m "stdout: %s" stdout);
      if stderr <> "" then Logs.debug (fun m -> m "stderr: %s" stderr);
      Ok { exit_code; stdout; stderr }
    end
  | WSIGNALED signal -> Error (sprintf "%s killed by signal %d" cmd signal)
  | WSTOPPED signal -> Error (sprintf "%s stopped by signal %d" cmd signal)
