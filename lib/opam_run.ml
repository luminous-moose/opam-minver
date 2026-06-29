open Result_syntax

let run_opam args =
  let* r = Process.run "opam" args in
  if r.Process.exit_code = 0 then Ok () else Error r.Process.stderr
