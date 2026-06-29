let extract_missing_library = Opam_minver.Opam_build.extract_missing_library
let classify_error = Opam_minver.Opam_build.classify_error

let%test "extract_missing_library: simple dotted name" =
  extract_missing_library {|Error: Library "mirage-crypto-rng.unix" not found.|}
  = Some "mirage-crypto-rng.unix"

let%test "extract_missing_library: dotted clock name" =
  extract_missing_library
    {|Error: Library "bechamel.monotonic_clock" not found.|}
  = Some "bechamel.monotonic_clock"

let%test "extract_missing_library: non-error line returns None" =
  extract_missing_library "-> required by alias bench/all" = None

let%test "extract_missing_library: empty string returns None" =
  extract_missing_library "" = None

(* Real stderr captured from a build on ocaml 5.3.0 *)
let chacha20_stderr =
  {|File "bench/dune", line 11, characters 31-53:
11 |              mirage-crypto-rng mirage-crypto-rng.unix bench_runner))
                                    ^^^^^^^^^^^^^^^^^^^^^^
Error: Library "mirage-crypto-rng.unix" not found.
-> required by _build/default/bench/bench_bits30.exe
-> required by alias bench/all
-> required by alias default
File "bench/dune", line 5, characters 22-46:
5 |   (libraries bechamel bechamel.monotonic_clock))
                          ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Library "bechamel.monotonic_clock" not found.
-> required by library "bench_runner" in _build/default/bench
-> required by
   _build/default/bench/.bench_runner.objs/native/bench_common.cmx
-> required by _build/default/bench/bench_runner.a
-> required by alias bench/all
-> required by alias default|}

let%test "classify_error: two missing libraries from chacha20 stderr" =
  match classify_error chacha20_stderr with
  | Missing_libraries libs ->
      List.sort String.compare libs
      = [ "bechamel.monotonic_clock"; "mirage-crypto-rng.unix" ]
  | Build_failure _ -> false

let%test "classify_error: deduplicates repeated missing library" =
  let stderr =
    {|Error: Library "foo" not found.
Error: Library "foo" not found.|}
  in
  classify_error stderr = Missing_libraries [ "foo" ]

let%test "classify_error: no Error lines gives Build_failure" =
  match classify_error "compilation failed\nsome other output" with
  | Build_failure _ -> true
  | Missing_libraries _ -> false

let%test "classify_error: mixed errors gives Build_failure" =
  let stderr =
    {|Error: Library "foo" not found.
Error: Something else went wrong.|}
  in
  match classify_error stderr with
  | Build_failure _ -> true
  | Missing_libraries _ -> false
