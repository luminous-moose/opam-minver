open Opam_minver

let () = Process.am_test := true

let with_dune_project content f =
  let tmp = Filename.temp_file "opam_minver_test_dp_" "" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o700;
  let proj = Filename.concat tmp "dune-project" in
  let oc = open_out proj in
  output_string oc content;
  close_out oc;
  Fun.protect
    ~finally:(fun () ->
      Sys.remove proj;
      Unix.rmdir tmp)
    (fun () -> f tmp)

let without_dune_project f =
  let tmp = Filename.temp_file "opam_minver_test_dp_" "" in
  Sys.remove tmp;
  Unix.mkdir tmp 0o700;
  Fun.protect ~finally:(fun () -> Unix.rmdir tmp) (fun () -> f tmp)

(* ---- basic cases ---- *)

let%test "no dune-project → false" =
  without_dune_project @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok false

let%test "minimal dune-project, no stanza → false" =
  with_dune_project "(lang dune 3.0)\n(name mypackage)" @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok false

let%test "(generate_opam_files false) → false" =
  with_dune_project "(lang dune 3.0)\n(generate_opam_files false)" @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok false

let%test "(generate_opam_files) bare form → true" =
  with_dune_project "(lang dune 3.0)\n(generate_opam_files)" @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok true

let%test "(generate_opam_files true) explicit → true" =
  with_dune_project "(lang dune 3.0)\n(generate_opam_files true)" @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok true

let%test "unparseable content → Error" =
  with_dune_project "(((" @@ fun dir ->
  match Dune_project.generates_opam_files ~dir with
  | Error _ -> true
  | Ok _ -> false

(* ---- real examples from ~/.opam ---- *)

(* csexp 1.5.2: uses generate_opam_files true with ; comments inside *)
let%test "csexp 1.5.2 → true" =
  with_dune_project
    {|(lang dune 3.4)
(name csexp)
(version 1.5.2)
(generate_opam_files true)
(package
 (name csexp)
 (depends
;  (ppx_expect :with-test)
; Disabled because of a dependency cycle
   (ocaml (>= 4.03.0)))
 (synopsis "Parsing and printing of S-expressions in Canonical form"))|}
  @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok true

(* hex 1.5.0: no generate_opam_files stanza at all *)
let%test "hex 1.5.0 → false" =
  with_dune_project "(lang dune 1.0)\n(name hex)\n(version v1.5.0)" @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok false

(* ocaml-base-compiler build dir: very old dune lang, no stanza *)
let%test "ocaml-base-compiler build dune-project → false" =
  with_dune_project
    "(lang dune 1.10)\n(using experimental_building_ocaml_compiler_with_dune 0.1)"
  @@ fun dir ->
  Dune_project.generates_opam_files ~dir = Ok false
