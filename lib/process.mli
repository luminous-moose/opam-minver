type t = {
  exit_code : int;
  stdout : string;
  stderr : string;
}

(* Set to true in test modules to assert that no subprocess is ever
   spawned. run will raise Failure if called while this is set. *)
val am_test : bool ref

(* [run ?cwd cmd args] executes [cmd] with [args], capturing stdout and
  stderr. If [cwd] is given the child process runs in that directory. *)
val run : ?cwd:string -> string -> string list -> (t, string) Stdlib.result
