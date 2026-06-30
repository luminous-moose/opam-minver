open Opam_minver

(* ------------------------------------------------------------------ *)
(* In-memory: lookup / record                                          *)
(* ------------------------------------------------------------------ *)

let%test "empty state: lookup returns Unknown" =
  let t = State.empty () in
  State.lookup t ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" = `Unknown

let%test "record Pass then lookup returns Pass" =
  let t = State.empty () in
  State.record t ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" `Pass;
  State.lookup t ~dep:"ocaml" ~ocaml_version:None ~version:"5.0.0" = `Pass

let%test "record Fail then lookup returns Fail" =
  let t = State.empty () in
  State.record t ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0" `Fail;
  State.lookup t ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0" = `Fail

let%test "record overwrites previous result" =
  let t = State.empty () in
  State.record t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" `Fail;
  State.record t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" `Pass;
  State.lookup t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" = `Pass

let%test "different versions are independent" =
  let t = State.empty () in
  State.record t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" `Fail;
  State.record t ~dep:"pkg" ~ocaml_version:None ~version:"2.0.0" `Pass;
  State.lookup t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" = `Fail
  && State.lookup t ~dep:"pkg" ~ocaml_version:None ~version:"2.0.0" = `Pass

let%test "composite key does not collide with simple key" =
  let t = State.empty () in
  State.record t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" `Pass;
  State.record t ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0" `Fail;
  State.lookup t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" = `Pass
  && State.lookup t ~dep:"pkg" ~ocaml_version:(Some "ocaml4") ~version:"1.0.0" = `Fail

(* ------------------------------------------------------------------ *)
(* In-memory: combined_done / record_combined                          *)
(* ------------------------------------------------------------------ *)

let%test "combined_done on empty state returns false" =
  let t = State.empty () in
  not (State.combined_done t "ocaml4" "fp1")

let%test "combined_done after record_combined with matching fingerprint" =
  let t = State.empty () in
  State.record_combined t "ocaml4" "fp1";
  State.combined_done t "ocaml4" "fp1"

let%test "combined_done with wrong fingerprint returns false" =
  let t = State.empty () in
  State.record_combined t "ocaml4" "fp1";
  not (State.combined_done t "ocaml4" "fp2")

let%test "combined_done after overwrite: old fingerprint gone, new present" =
  let t = State.empty () in
  State.record_combined t "ocaml4" "fp1";
  State.record_combined t "ocaml4" "fp2";
  (not (State.combined_done t "ocaml4" "fp1"))
  && State.combined_done t "ocaml4" "fp2"

let%test "combined_done is independent per ocaml_key" =
  let t = State.empty () in
  State.record_combined t "ocaml4" "fp1";
  (not (State.combined_done t "ocaml5" "fp1"))
  && State.combined_done t "ocaml4" "fp1"

(* ------------------------------------------------------------------ *)
(* File I/O helpers                                                    *)
(* ------------------------------------------------------------------ *)

let with_temp_dir f =
  let path = Filename.temp_file "opam_minver_test_state_" "" in
  Sys.remove path;
  Unix.mkdir path 0o700;
  Fun.protect
    ~finally:(fun () ->
      List.iter
        (fun name ->
          try Sys.remove (Filename.concat path name) with Sys_error _ -> ())
        [ "opam-minver.json"; "opam-minver.json.tmp" ];
      Unix.rmdir path)
    (fun () -> f path)

(* ------------------------------------------------------------------ *)
(* File I/O edge cases                                                 *)
(* ------------------------------------------------------------------ *)

let%test "load from missing file returns empty state" =
  with_temp_dir (fun dir ->
      let t = State.load ~dir in
      State.lookup t ~dep:"anything" ~ocaml_version:None ~version:"1.0.0"
      = `Unknown)

let%test "load from unparseable JSON returns empty state" =
  with_temp_dir (fun dir ->
      let path = Filename.concat dir "opam-minver.json" in
      Out_channel.with_open_text path (fun oc ->
          Out_channel.output_string oc "not json {{{{");
      let t = State.load ~dir in
      State.lookup t ~dep:"anything" ~ocaml_version:None ~version:"1.0.0"
      = `Unknown)

let%test "save to non-existent directory does not raise" =
  let t = State.empty () in
  State.record t ~dep:"pkg" ~ocaml_version:None ~version:"1.0.0" `Pass;
  State.save ~dir:"/no/such/dir/opam_minver_test" t;
  true

(* ------------------------------------------------------------------ *)
(* JSON output                                                         *)
(* ------------------------------------------------------------------ *)

let%expect_test "saved JSON: results and combined fields" =
  with_temp_dir (fun dir ->
      let t = State.empty () in
      State.record t ~dep:"ocaml" ~ocaml_version:None ~version:"4.08.0" `Fail;
      State.record t ~dep:"ocaml" ~ocaml_version:None ~version:"4.14.0" `Pass;
      State.record t ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
        ~version:"1.0.0" `Fail;
      State.record t ~dep:"cmdliner" ~ocaml_version:(Some "ocaml4")
        ~version:"1.1.0" `Pass;
      State.record_combined t "ocaml4" "cmdliner=1.1.0";
      State.save ~dir t;
      print_string
        (In_channel.with_open_text
           (Filename.concat dir "opam-minver.json")
           In_channel.input_all));
  [%expect {|
    {
      "results": {
        "cmdliner@ocaml4": {
          "1.0.0": "fail",
          "1.1.0": "pass"
        },
        "ocaml": {
          "4.08.0": "fail",
          "4.14.0": "pass"
        }
      },
      "combined": {
        "ocaml4": "cmdliner=1.1.0"
      }
    }
    |}]

let%expect_test "combined round-trip through save/load" =
  with_temp_dir (fun dir ->
      let t = State.empty () in
      State.record_combined t "ocaml4" "pkg=1.0.0,dune=3.0.0";
      State.record_combined t "ocaml5" "pkg=2.0.0,dune=3.0.0";
      State.save ~dir t;
      let t2 = State.load ~dir in
      Printf.printf "ocaml4/exact: %b\n"
        (State.combined_done t2 "ocaml4" "pkg=1.0.0,dune=3.0.0");
      Printf.printf "ocaml5/exact: %b\n"
        (State.combined_done t2 "ocaml5" "pkg=2.0.0,dune=3.0.0");
      Printf.printf "ocaml4/wrong: %b\n"
        (State.combined_done t2 "ocaml4" "pkg=9.9.9,dune=3.0.0"));
  [%expect {|
    ocaml4/exact: true
    ocaml5/exact: true
    ocaml4/wrong: false
    |}]
