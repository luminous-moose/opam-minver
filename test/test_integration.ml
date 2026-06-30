(* Integration tests for Runner functions.  State is pre-populated with
   pass/fail results so binary_search never fires the real subprocess test
   function.  The injectable probe_env stubs out all four opam side-effects. *)
open Opam_minver
open Manifest

let () = Process.am_test := true
let v = Version.of_string
let dep name bound : dep = { name; scope = Runtime; bound }

let stub_env ~ocaml_vs ~current ~dep_vs =
  {
    Runner.ocaml_versions = (fun () -> Ok (List.map v ocaml_vs));
    current_ocaml = (fun () -> Ok (v current));
    dep_versions =
      (fun pkg ->
        match List.assoc_opt pkg dep_vs with
        | Some vs -> Ok (Versions (List.map v vs))
        | None -> Error ("no versions for " ^ pkg));
    remove_all_pins = (fun _ -> Ok ());
    run_combined_validation =
      (fun ?note:_ ?on_fail:_ ~package:_ ~dir:_ ~verbose:_ _ _ _ -> Ok ());
  }

let show_probe r =
  let show label = function
    | None -> Printf.printf "%s: none\n" label
    | Some (min, test) ->
        Printf.printf "%s: min=%s test=%s\n" label (Version.to_string min)
          (Version.to_string test)
  in
  show "ocaml4" r.Runner.ocaml4_min_and_test;
  show "ocaml5" r.Runner.ocaml5_min_and_test

let with_opam_file deps_str f =
  let content =
    Printf.sprintf {|opam-version: "2.0"
synopsis: "test"
depends: [
%s
]
|}
      deps_str
  in
  let path = Filename.temp_file "opam_minver_test_integration_" ".opam" in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let read_file path =
  let ic = open_in path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s

(* ------------------------------------------------------------------ *)
(* probe_ocaml_versions                                                *)
(* ------------------------------------------------------------------ *)

(* Every OCaml version returned by env.ocaml_versions must be recorded in
   State so binary_search never fires the real test_compiler_version
   function.  dir is unused since State.save is only reachable from the
   test closure, which never executes. *)

let%expect_test
    "probe_ocaml_versions: both pass, current OCaml 5, o5 test overridden to \
     current" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.1.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.2.0" `Pass;
  (* 5.2.0 is the running OCaml but is not in the repo list; binary search
     finds max 5.1.0, but probe_ocaml_versions overrides the o5 test version
     to current so deps are tested under the actual running compiler. *)
  let env =
    stub_env
      ~ocaml_vs:[ "4.14.0"; "5.0.0"; "5.1.0"; "5.2.0" ]
      ~current:"5.2.0" ~dep_vs:[]
  in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       None
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {|
    ocaml4: min=4.14.0 test=4.14.0
    ocaml5: min=5.0.0 test=5.2.0
    |}]

let%expect_test
    "probe_ocaml_versions: both pass, current OCaml 4, o4 test overridden to \
     current" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.12.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.1.0" `Pass;
  let env =
    stub_env
      ~ocaml_vs:[ "4.12.0"; "4.14.0"; "5.0.0"; "5.1.0" ]
      ~current:"4.14.0" ~dep_vs:[]
  in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       None
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {|
    ocaml4: min=4.12.0 test=4.14.0
    ocaml5: min=5.0.0 test=5.1.0
    |}]

let%expect_test "probe_ocaml_versions: only OCaml 4 passes" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Fail;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.2.0" `Fail;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"5.2.0" ~dep_vs:[]
  in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       None
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {| error: project does not build and pass tests with current compiler version |}]

let%expect_test "probe_ocaml_versions: only OCaml 4 passes, OCaml 4 current" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Fail;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"4.14.0" ~dep_vs:[]
  in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       None
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    ocaml4: min=4.14.0 test=4.14.0
    ocaml5: none
    |}]

let%expect_test
    "probe_ocaml_versions: only OCaml 5 passes, no current override in \
     single-branch case" =
  (* The current-version override only fires in the both-pass branch.
     When only one side passes the raw binary-search result is used. *)
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Fail;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.2.0" `Pass;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"5.2.0" ~dep_vs:[]
  in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       None
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    ocaml4: none
    ocaml5: min=5.0.0 test=5.0.0
    |}]

let%expect_test "probe_ocaml_versions: no compilers pass returns error" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Fail;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Fail;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.2.0" `Fail;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"5.2.0" ~dep_vs:[]
  in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       None
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {| error: project does not build and pass tests with current compiler version |}]

let%expect_test
    "probe_ocaml_versions: current passes but all repo versions fail" =
  (* current (5.2.0) is recorded as Pass so test_compiler_version returns true.
     All versions available in the repo (4.14.0, 5.0.0) are Fail in state, so
     both binary searches return None, hitting the None, None error branch. *)
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.2.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Fail;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Fail;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"5.2.0" ~dep_vs:[]
  in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       None
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {| error: No OCaml compilers pass |}]

let%expect_test "probe_ocaml_versions: ocamldep constraint filters search range"
    =
  (* 4.08.0 is in env.ocaml_versions but excluded by At_least "4.14.0".
     It is deliberately absent from State: if it were probed, binary_search
     would call the real test function and the subprocess would fail. *)
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  let env =
    stub_env
      ~ocaml_vs:[ "4.08.0"; "4.14.0"; "5.0.0" ]
      ~current:"5.0.0" ~dep_vs:[]
  in
  let ocamldep = Some (dep "ocaml" (Simple (At_least "4.14.0"))) in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       ocamldep
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {|
    ocaml4: min=4.14.0 test=4.14.0
    ocaml5: min=5.0.0 test=5.0.0
    |}]

let%expect_test
    "probe_ocaml_versions: At_least OCaml 5 bound, bound version passes, 1 \
     OCaml 5 probe" =
  (* At_least "5.1.0" filters out 4.x and 5.0.0, leaving ocaml5=[5.1.0;5.2.0;5.3.0].
     The optimization probes 5.1.0 first (index 0); it passes, so binary_search
     is skipped entirely.  5.2.0 is deliberately absent from State: if it were
     probed, binary_search would call the real test function and the subprocess
     would fail.  Without the optimization, binary_search would visit 5.2.0
     first (mid=1) and then 5.1.0, requiring 5.2.0 to be in State as well. *)
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.3.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.1.0" `Pass;
  let env =
    stub_env
      ~ocaml_vs:[ "4.14.0"; "5.0.0"; "5.1.0"; "5.2.0"; "5.3.0" ]
      ~current:"5.3.0" ~dep_vs:[]
  in
  let ocamldep = Some (dep "ocaml" (Simple (At_least "5.1.0"))) in
  (match
     Runner.probe_ocaml_versions env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       ocamldep
   with
  | Ok r -> show_probe r
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    ocaml4: none
    ocaml5: min=5.1.0 test=5.1.0
    |}]

(* ------------------------------------------------------------------ *)
(* probe_deps                                                          *)
(* ------------------------------------------------------------------ *)

(* probe_deps accumulates results in reverse (list cons); tests call
   List.rev before display so output order matches input dep order. *)

let%expect_test "probe_deps: single dep, floor found" =
  let state = State.empty () in
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.0.0" `Fail;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.1.0" `Pass;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.2.0" `Pass;
  let env =
    stub_env ~ocaml_vs:[] ~current:"4.14.0"
      ~dep_vs:[ ("cmdliner", [ "1.0.0"; "1.1.0"; "1.2.0" ]) ]
  in
  let deps = [ dep "cmdliner" (Simple Unconstrained) ] in
  (match
     Runner.probe_deps env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       `Ocaml4 (v "4.14.0") deps
   with
  | Ok results ->
      List.iter
        (fun (d, ver) ->
          Printf.printf "%s: %s\n" d.name (Version.to_string ver))
        (List.rev results)
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    cmdliner: 1.1.0
    |}]

let%expect_test "probe_deps: multiple deps all found" =
  let state = State.empty () in
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.0.0" `Fail;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.1.0" `Pass;
  State.record state ~dep:"dune" ~ocaml_version:(Some "ocaml4") ~version:"3.0.0"
    `Pass;
  let env =
    stub_env ~ocaml_vs:[] ~current:"4.14.0"
      ~dep_vs:[ ("cmdliner", [ "1.0.0"; "1.1.0" ]); ("dune", [ "3.0.0" ]) ]
  in
  let deps =
    [ dep "cmdliner" (Simple Unconstrained); dep "dune" (Simple Unconstrained) ]
  in
  (match
     Runner.probe_deps env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       `Ocaml4 (v "4.14.0") deps
   with
  | Ok results ->
      List.iter
        (fun (d, ver) ->
          Printf.printf "%s: %s\n" d.name (Version.to_string ver))
        (List.rev results)
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    cmdliner: 1.1.0
    dune: 3.0.0
    |}]

let%test "probe_deps: no passing version returns error" =
  let state = State.empty () in
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"2.0.0"
    `Fail;
  let env =
    stub_env ~ocaml_vs:[] ~current:"4.14.0"
      ~dep_vs:[ ("foo", [ "1.0.0"; "2.0.0" ]) ]
  in
  let deps = [ dep "foo" (Simple Unconstrained) ] in
  match
    Runner.probe_deps env
      { Probe.state; dir = "."; package = "test-package"; verbose = false }
      `Ocaml4 (v "4.14.0") deps
  with
  | Error msg -> String.equal msg "no version found for foo"
  | Ok _ -> false

let%test "probe_deps: skip dep is not probed" =
  let state = State.empty () in
  State.record state ~dep:"bar" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Pass;
  (* dep_versions asserts false for "foo": if the Skip dep reaches dep_versions
     the test will raise Assert_failure, proving the filter works. *)
  let env =
    {
      Runner.ocaml_versions = (fun () -> Ok []);
      current_ocaml = (fun () -> Ok (v "4.14.0"));
      dep_versions =
        (fun pkg ->
          if String.equal pkg "foo" then assert false
          else Ok (Versions [ v "1.0.0" ]));
      remove_all_pins = (fun _ -> Ok ());
      run_combined_validation =
        (fun ?note:_ ?on_fail:_ ~package:_ ~dir:_ ~verbose:_ _ _ _ -> Ok ());
    }
  in
  let deps =
    [
      dep "foo" (Skip {|"foo" {os = "linux"}|});
      dep "bar" (Simple Unconstrained);
    ]
  in
  match
    Runner.probe_deps env
      { Probe.state; dir = "."; package = "test-package"; verbose = false }
      `Ocaml4 (v "4.14.0") deps
  with
  | Ok results ->
      List.length results = 1 && String.equal (fst (List.hd results)).name "bar"
  | Error _ -> false

let%test "probe_deps: with-doc dep is not probed" =
  let state = State.empty () in
  State.record state ~dep:"bar" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Pass;
  (* dep_versions asserts false for "odoc": if the With_doc dep reaches
     dep_versions the test will raise Assert_failure, proving the filter works. *)
  let env =
    {
      Runner.ocaml_versions = (fun () -> Ok []);
      current_ocaml = (fun () -> Ok (v "4.14.0"));
      dep_versions =
        (fun pkg ->
          if String.equal pkg "odoc" then assert false
          else Ok (Versions [ v "1.0.0" ]));
      remove_all_pins = (fun _ -> Ok ());
      run_combined_validation =
        (fun ?note:_ ?on_fail:_ ~package:_ ~dir:_ ~verbose:_ _ _ _ -> Ok ());
    }
  in
  let deps =
    [
      { name = "odoc"; scope = With_doc; bound = Simple Unconstrained };
      dep "bar" (Simple Unconstrained);
    ]
  in
  match
    Runner.probe_deps env
      { Probe.state; dir = "."; package = "test-package"; verbose = false }
      `Ocaml4 (v "4.14.0") deps
  with
  | Ok results ->
      List.length results = 1 && String.equal (fst (List.hd results)).name "bar"
  | Error _ -> false

let%expect_test "probe_deps: base package is skipped with message" =
  let state = State.empty () in
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.1.0" `Pass;
  let env =
    {
      Runner.ocaml_versions = (fun () -> Ok []);
      current_ocaml = (fun () -> Ok (v "4.14.0"));
      dep_versions =
        (fun pkg ->
          if String.equal pkg "seq" then Ok Opam_versions.Contains_base
          else Ok (Opam_versions.Versions [ v "1.1.0" ]));
      remove_all_pins = (fun _ -> Ok ());
      run_combined_validation =
        (fun ?note:_ ?on_fail:_ ~package:_ ~dir:_ ~verbose:_ _ _ _ -> Ok ());
    }
  in
  let deps =
    [ dep "seq" (Simple Unconstrained); dep "cmdliner" (Simple Unconstrained) ]
  in
  (match
     Runner.probe_deps env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       `Ocaml4 (v "4.14.0") deps
   with
  | Ok results ->
      List.iter
        (fun (d, ver) ->
          Printf.printf "%s: %s\n" d.name (Version.to_string ver))
        (List.rev results)
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {| cmdliner: 1.1.0 |}]

let%expect_test
    "probe_deps: unconstrained, 5 versions, floor at index 1, 3 probes" =
  (* No existing bound; binary_search visits mid=2 (1.2.0 passes), recurses
     left to mid=0 (1.0.0 fails), then mid=1 (1.1.0 passes).  Versions 1.3.0
     and 1.4.0 are never visited.  The optimization has no effect here since
     the bound is Unconstrained. *)
  let state = State.empty () in
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.1.0"
    `Pass;
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.2.0"
    `Pass;
  let env =
    stub_env ~ocaml_vs:[] ~current:"4.14.0"
      ~dep_vs:[ ("foo", [ "1.0.0"; "1.1.0"; "1.2.0"; "1.3.0"; "1.4.0" ]) ]
  in
  let deps = [ dep "foo" (Simple Unconstrained) ] in
  (match
     Runner.probe_deps env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       `Ocaml4 (v "4.14.0") deps
   with
  | Ok results ->
      List.iter
        (fun (d, ver) ->
          Printf.printf "%s: %s\n" d.name (Version.to_string ver))
        (List.rev results)
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {| foo: 1.1.0 |}]

let%expect_test "probe_deps: At_least bound, bound version passes, 1 probe" =
  (* filter_dep_versions(At_least "1.1.0") yields [1.1.0; 1.2.0; 1.3.0; 1.4.0].
     The optimization probes 1.1.0 first (index 0); it passes, so binary_search
     is skipped entirely.  Only 1.1.0 need be in State; 1.2.0–1.4.0 are never
     visited.  Without the optimization, binary_search would visit 1.2.0 and
     then 1.1.0 for 2 probes. *)
  let state = State.empty () in
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.1.0"
    `Pass;
  let env =
    stub_env ~ocaml_vs:[] ~current:"4.14.0"
      ~dep_vs:[ ("foo", [ "1.0.0"; "1.1.0"; "1.2.0"; "1.3.0"; "1.4.0" ]) ]
  in
  let deps = [ dep "foo" (Simple (At_least "1.1.0")) ] in
  (match
     Runner.probe_deps env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       `Ocaml4 (v "4.14.0") deps
   with
  | Ok results ->
      List.iter
        (fun (d, ver) ->
          Printf.printf "%s: %s\n" d.name (Version.to_string ver))
        (List.rev results)
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {| foo: 1.1.0 |}]

let%expect_test "probe_deps: At_least bound, bound version fails, 3 probes" =
  (* filter_dep_versions(At_least "1.1.0") yields [1.1.0; 1.2.0; 1.3.0; 1.4.0].
     The optimization probes 1.1.0 first (index 0); it fails, so binary_search
     runs with versions_map.(0) already set to Fail.  binary_search then visits
     mid=1 (1.2.0 fails) and mid=2 (1.3.0 passes) for 3 probes total.
     Without the optimization, binary_search would skip 1.1.0 entirely and
     visit only 1.2.0 and 1.3.0 for 2 probes. This case is expected to be
     significantly less common than the case above. *)
  let state = State.empty () in
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.1.0"
    `Fail;
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.2.0"
    `Fail;
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.3.0"
    `Pass;
  let env =
    stub_env ~ocaml_vs:[] ~current:"4.14.0"
      ~dep_vs:[ ("foo", [ "1.0.0"; "1.1.0"; "1.2.0"; "1.3.0"; "1.4.0" ]) ]
  in
  let deps = [ dep "foo" (Simple (At_least "1.1.0")) ] in
  (match
     Runner.probe_deps env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       `Ocaml4 (v "4.14.0") deps
   with
  | Ok results ->
      List.iter
        (fun (d, ver) ->
          Printf.printf "%s: %s\n" d.name (Version.to_string ver))
        (List.rev results)
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {| foo: 1.3.0 |}]

let%expect_test "probe_deps: existing bound filters candidate versions" =
  (* 1.0.0 is excluded by filter_dep_versions(At_least "1.1.0") before State
     lookup.  The optimization probes 1.1.0 first; since it passes, binary_search
     is skipped entirely, so 1.2.0 is never visited and need not be in State. *)
  let state = State.empty () in
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.1.0" `Pass;
  let env =
    stub_env ~ocaml_vs:[] ~current:"4.14.0"
      ~dep_vs:[ ("cmdliner", [ "1.0.0"; "1.1.0"; "1.2.0" ]) ]
  in
  let deps = [ dep "cmdliner" (Simple (At_least "1.1.0")) ] in
  (match
     Runner.probe_deps env
       { Probe.state; dir = "."; package = "test-package"; verbose = false }
       `Ocaml4 (v "4.14.0") deps
   with
  | Ok results ->
      List.iter
        (fun (d, ver) ->
          Printf.printf "%s: %s\n" d.name (Version.to_string ver))
        (List.rev results)
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    cmdliner: 1.1.0
    |}]

(* ------------------------------------------------------------------ *)
(* compute_bounds                                                      *)
(* ------------------------------------------------------------------ *)

let%expect_test
    "compute_bounds: both o4 and o5, equal floors collapse to Simple" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.0.0" `Fail;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
    ~version:"1.1.0" `Pass;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml5")
    ~version:"1.0.0" `Fail;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml5")
    ~version:"1.1.0" `Pass;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"5.0.0"
      ~dep_vs:[ ("cmdliner", [ "1.0.0"; "1.1.0" ]) ]
  in
  let deps =
    [
      dep "ocaml" (Simple Unconstrained); dep "cmdliner" (Simple Unconstrained);
    ]
  in
  (match
     Runner.compute_bounds env state "." ~verbose:false ~package:"test-package"
       deps
   with
  | Ok new_deps -> List.iter (fun d -> print_endline (dep_to_string d)) new_deps
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    "ocaml" {>= "4.14.0"}
    "cmdliner" {>= "1.1.0"}
    |}]

let%expect_test "compute_bounds: no ocaml dep in manifest, one is synthesised" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"fmt" ~ocaml_version:(Some "ocaml5") ~version:"0.9.0"
    `Pass;
  let env =
    stub_env ~ocaml_vs:[ "5.0.0" ] ~current:"5.0.0"
      ~dep_vs:[ ("fmt", [ "0.9.0" ]) ]
  in
  let deps = [ dep "fmt" (Simple Unconstrained) ] in
  (match
     Runner.compute_bounds env state "." ~verbose:false ~package:"test-package"
       deps
   with
  | Ok new_deps -> List.iter (fun d -> print_endline (dep_to_string d)) new_deps
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    "ocaml" {>= "5.0.0"}
    "fmt" {>= "0.9.0"}
    |}]

let%expect_test "compute_bounds: only OCaml 4 passes, simple bounds" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Fail;
  State.record state ~dep:"foo" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Pass;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"4.14.0"
      ~dep_vs:[ ("foo", [ "1.0.0" ]) ]
  in
  let deps =
    [ dep "ocaml" (Simple Unconstrained); dep "foo" (Simple Unconstrained) ]
  in
  (match
     Runner.compute_bounds env state "." ~verbose:false ~package:"test-package"
       deps
   with
  | Ok new_deps -> List.iter (fun d -> print_endline (dep_to_string d)) new_deps
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {|
    "ocaml" {>= "4.14.0" & < "5.0.0"}
    "foo" {>= "1.0.0"}
    |}]

let%expect_test "compute_bounds: differing o4 and o5 floors produce Ocaml_split"
    =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.1.0"
    `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.1.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.2.0"
    `Pass;
  let env =
    stub_env ~ocaml_vs:[ "4.14.0"; "5.0.0" ] ~current:"5.0.0"
      ~dep_vs:[ ("pkg", [ "1.0.0"; "1.1.0"; "1.2.0" ]) ]
  in
  let deps =
    [ dep "ocaml" (Simple Unconstrained); dep "pkg" (Simple Unconstrained) ]
  in
  (match
     Runner.compute_bounds env state "." ~verbose:false ~package:"test-package"
       deps
   with
  | Ok new_deps -> List.iter (fun d -> print_endline (dep_to_string d)) new_deps
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect {|
    "ocaml" {>= "4.14.0"}
    "pkg" {>= "1.2.0"}
    |}]

let%expect_test
    "compute_bounds: differing o4 and o5 compiler floors produce Ocaml_split" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Fail;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.1.0" `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.1.0"
    `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.1.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.2.0"
    `Pass;
  let env =
    stub_env
      ~ocaml_vs:[ "4.14.0"; "5.0.0"; "5.1.0" ]
      ~current:"5.1.0"
      ~dep_vs:[ ("pkg", [ "1.0.0"; "1.1.0"; "1.2.0" ]) ]
  in
  let deps =
    [ dep "ocaml" (Simple Unconstrained); dep "pkg" (Simple Unconstrained) ]
  in
  (match
     Runner.compute_bounds env state "." ~verbose:false ~package:"test-package"
       deps
   with
  | Ok new_deps -> List.iter (fun d -> print_endline (dep_to_string d)) new_deps
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {|
    "ocaml" {>= "4.14.0" & < "5.0.0" | >= "5.1.0"}
    "pkg" {>= "1.2.0"}
    |}]

let%expect_test
    "compute_bounds: collapse validation fires when dep versions split" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.1.0"
    `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.1.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.2.0"
    `Pass;
  let calls = ref [] in
  let env =
    {
      Runner.ocaml_versions = (fun () -> Ok (List.map v [ "4.14.0"; "5.0.0" ]));
      current_ocaml = (fun () -> Ok (v "5.0.0"));
      dep_versions =
        (fun _ ->
          Ok (Opam_versions.Versions (List.map v [ "1.0.0"; "1.1.0"; "1.2.0" ])));
      remove_all_pins = (fun _ -> Ok ());
      run_combined_validation =
        (fun ?(note = "")
          ?on_fail:_
          ~package:_
          ~dir:_
          ~verbose:_
          ocamlv
          _ocaml_min
          dep_results
        ->
          let major = match ocamlv with `Ocaml4 -> 4 | `Ocaml5 -> 5 in
          let pins =
            dep_results
            |> List.filter_map (fun (dep, ver) ->
                if dep.Manifest.name = "ocaml" then None
                else Some (dep.Manifest.name ^ "=" ^ Version.to_string ver))
            |> List.sort String.compare |> String.concat ","
          in
          let note_s = if note = "" then "" else " [" ^ note ^ "]" in
          calls := Printf.sprintf "OCaml%d%s: %s" major note_s pins :: !calls;
          Ok ());
    }
  in
  let deps =
    [ dep "ocaml" (Simple Unconstrained); dep "pkg" (Simple Unconstrained) ]
  in
  (match
     Runner.compute_bounds env state "." ~verbose:false ~package:"test-package"
       deps
   with
  | Ok _ -> ()
  | Error e -> Printf.printf "error: %s\n" e);
  List.iter print_endline (List.rev !calls);
  [%expect
    {|
    OCaml4: pkg=1.1.0
    OCaml5: pkg=1.2.0
    OCaml4 [collapsed bounds]: pkg=1.2.0
    |}]

let%expect_test
    "compute_bounds: collapse validation skipped when dep versions match" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.1.0"
    `Pass;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.0.0"
    `Fail;
  State.record state ~dep:"pkg" ~ocaml_version:(Some "ocaml5") ~version:"1.1.0"
    `Pass;
  let calls = ref [] in
  let env =
    {
      Runner.ocaml_versions = (fun () -> Ok (List.map v [ "4.14.0"; "5.0.0" ]));
      current_ocaml = (fun () -> Ok (v "5.0.0"));
      dep_versions =
        (fun _ -> Ok (Opam_versions.Versions (List.map v [ "1.0.0"; "1.1.0" ])));
      remove_all_pins = (fun _ -> Ok ());
      run_combined_validation =
        (fun ?(note = "")
          ?on_fail:_
          ~package:_
          ~dir:_
          ~verbose:_
          ocamlv
          _ocaml_min
          dep_results
        ->
          let major = match ocamlv with `Ocaml4 -> 4 | `Ocaml5 -> 5 in
          let pins =
            dep_results
            |> List.filter_map (fun (dep, ver) ->
                if dep.Manifest.name = "ocaml" then None
                else Some (dep.Manifest.name ^ "=" ^ Version.to_string ver))
            |> List.sort String.compare |> String.concat ","
          in
          let note_s = if note = "" then "" else " [" ^ note ^ "]" in
          calls := Printf.sprintf "OCaml%d%s: %s" major note_s pins :: !calls;
          Ok ());
    }
  in
  let deps =
    [ dep "ocaml" (Simple Unconstrained); dep "pkg" (Simple Unconstrained) ]
  in
  (match
     Runner.compute_bounds env state "." ~verbose:false ~package:"test-package"
       deps
   with
  | Ok _ -> ()
  | Error e -> Printf.printf "error: %s\n" e);
  List.iter print_endline (List.rev !calls);
  [%expect {|
    OCaml4: pkg=1.1.0
    OCaml5: pkg=1.1.0
    |}]

(* ------------------------------------------------------------------ *)
(* run_with                                                            *)
(* ------------------------------------------------------------------ *)

let%expect_test "run_with write_out:false prints dep section to stdout" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml5")
    ~version:"1.1.0" `Pass;
  let env =
    stub_env ~ocaml_vs:[ "5.0.0" ] ~current:"5.0.0"
      ~dep_vs:[ ("cmdliner", [ "1.1.0" ]) ]
  in
  with_opam_file {|  "ocaml"
  "cmdliner"|} @@ fun path ->
  let dir = Filename.dirname path in
  let manifest =
    match Manifest_parse.read path with Ok m -> m | Error e -> failwith e
  in
  (match
     Runner.run_with env state dir manifest ~verbose:false ~write_out:false
   with
  | Ok () -> ()
  | Error e -> Printf.printf "error: %s\n" e);
  [%expect
    {|
    Dep bounds found:
    depends: [
      "ocaml" {>= "5.0.0"}
      "cmdliner" {>= "1.1.0"}
    ]
    |}]

let%expect_test "run_with write_out:true rewrites the opam file in place" =
  let state = State.empty () in
  State.record state ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.record state ~dep:"cmdliner" ~ocaml_version:(Some "ocaml5")
    ~version:"1.1.0" `Pass;
  let env =
    stub_env ~ocaml_vs:[ "5.0.0" ] ~current:"5.0.0"
      ~dep_vs:[ ("cmdliner", [ "1.1.0" ]) ]
  in
  with_opam_file {|  "ocaml"
  "cmdliner"|} @@ fun path ->
  let dir = Filename.dirname path in
  let manifest =
    match Manifest_parse.read path with Ok m -> m | Error e -> failwith e
  in
  (match
     Runner.run_with env state dir manifest ~verbose:false ~write_out:true
   with
  | Ok () -> ()
  | Error e -> Printf.printf "error: %s\n" e);
  print_string (read_file path);
  [%expect
    {|
    Opam file written.
    opam-version: "2.0"
    synopsis: "test"
    depends: [
      "ocaml" {>= "5.0.0"}
      "cmdliner" {>= "1.1.0"}
    ]
    |}]

(* ------------------------------------------------------------------ *)
(* State save/load                                                      *)
(* ------------------------------------------------------------------ *)

let with_temp_dir f =
  let path = Filename.temp_file "opam_minver_test_state_" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () ->
      (try Sys.remove (Filename.concat path "opam-minver.json")
       with Sys_error _ -> ());
      Unix.rmdir path)
    (fun () -> f path)

let%expect_test "state round-trip: simple and composite keys survive save/load"
    =
  with_temp_dir (fun dir ->
      let t = State.empty () in
      State.record t ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
      State.record t ~dep:"ocaml" ~ocaml_version:None ~version:"4.08.0" `Fail;
      State.record t ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
        ~version:"1.0.0" `Fail;
      State.record t ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
        ~version:"1.1.0" `Pass;
      State.record t ~dep:"cmdliner" ~ocaml_version:(Some "ocaml5")
        ~version:"1.2.0" `Pass;
      State.save ~dir t;
      let t2 = State.load ~dir in
      let show label dep ocaml_version version =
        let r = State.lookup t2 ~dep ~ocaml_version ~version in
        Printf.printf "%s: %s\n" label
          (match r with
          | `Pass -> "pass"
          | `Fail -> "fail"
          | `Unknown -> "unknown")
      in
      show "ocaml/4.14.0" "ocaml" None "4.14.0";
      show "ocaml/4.08.0" "ocaml" None "4.08.0";
      show "cmdliner@ocaml4/1.0.0" "cmdliner" (Some "ocaml4") "1.0.0";
      show "cmdliner@ocaml4/1.1.0" "cmdliner" (Some "ocaml4") "1.1.0";
      show "cmdliner@ocaml5/1.2.0" "cmdliner" (Some "ocaml5") "1.2.0";
      show "cmdliner@ocaml4/9.9.9 (absent)" "cmdliner" (Some "ocaml4") "9.9.9");
  [%expect
    {|
    ocaml/4.14.0: pass
    ocaml/4.08.0: fail
    cmdliner@ocaml4/1.0.0: fail
    cmdliner@ocaml4/1.1.0: pass
    cmdliner@ocaml5/1.2.0: pass
    cmdliner@ocaml4/9.9.9 (absent): unknown
    |}]
