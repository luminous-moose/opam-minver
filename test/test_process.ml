open Opam_minver

let expected_size = 32 * 4096 * 2

(* happy path *)

let%test "large output - exit code 0" =
  match Process.run "./big_output.exe" [] with
  | Error _ -> false
  | Ok r -> r.Process.exit_code = 0

let%test "large output - stdout size" =
  match Process.run "./big_output.exe" [] with
  | Error _ -> false
  | Ok r -> String.length r.Process.stdout = expected_size

let%test "large output - stderr size" =
  match Process.run "./big_output.exe" [] with
  | Error _ -> false
  | Ok r -> String.length r.Process.stderr = expected_size

(* edge case *)

let%test "nonexistent binary gives Error" =
  match Process.run "/nonexistent/binary" [] with
  | Ok _ -> false
  | Error _ -> true

(* expect test *)

let%expect_test "large output sizes" =
  (match Process.run "./big_output.exe" [] with
  | Error msg -> Printf.printf "error: %s\n" msg
  | Ok r ->
      Printf.printf "exit_code: %d\n" r.Process.exit_code;
      Printf.printf "stdout_size: %d\n" (String.length r.Process.stdout);
      Printf.printf "stderr_size: %d\n" (String.length r.Process.stderr));
  [%expect
    {|
    exit_code: 0
    stdout_size: 262144
    stderr_size: 262144
    |}]
