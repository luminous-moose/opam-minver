open Printf
open Result_syntax

let vprintf verbose fmt = if verbose then printf fmt else ifprintf stdout fmt

type ctx = { state : State.t; dir : string; package : string; verbose : bool }

exception Missing_libraries of string list

(* Convert a build result to [Ok]/[Error], raising [Missing_libraries] for
   the corresponding [Opam_build] error variant. *)
let run_build_step result =
  match result with
  | Ok () -> Ok ()
  | Error (Opam_build.Missing_libraries libs) -> raise (Missing_libraries libs)
  | Error (Opam_build.Build_failure msg) -> Error msg

(* Do a binary search to find the lowest version in [lo..hi] for which test
   returns true, consulting and updating versions_map to avoid redundant calls.
   Most binary searches don't have such a map: the main uses of it here are
   resuming interrupted sessions, testing, and allowing [test_compiler] to find
   the highest tested passing version. Returns None if no version passes. *)
let rec binary_search versions versions_map lo hi test =
  if lo > hi then None
  else
    let mid = (lo + hi) / 2 in
    let r =
      match versions_map.(mid) with
      | `Pass -> true
      | `Fail -> false
      | `Unknown ->
          let r = test versions.(mid) in
          versions_map.(mid) <- (if r then `Pass else `Fail);
          r
    in
    if r then
      match binary_search versions versions_map lo (mid - 1) test with
      | None -> Some versions.(mid)
      | some -> some
    else binary_search versions versions_map (mid + 1) hi test

(* Test a single OCaml compiler version. Creates the switch if needed, installs
   deps, builds, and runs tests. Reads and writes to the state cache. *)
let test_compiler_version ?(save_on_fail = true) ctx version =
  let version_string = Version.to_string version in
  match
    State.lookup ctx.state ~dep:"ocaml" ~ocaml_version:None
      ~version:version_string
  with
  | `Pass -> true
  | `Fail -> false
  | `Unknown -> begin
      vprintf ctx.verbose "Testing OCaml %s...\n%!" version_string;
      let switch = Opam_switch.prefix ^ "ocaml-" ^ version_string in
      let result =
        let* () =
          Opam_switch.find_or_create ~name:switch ~compiler:version_string
        in
        vprintf ctx.verbose "Created OCaml %s.\n%!" version_string;
        let* () = Opam_install.deps ~switch ~dir:ctx.dir in
        vprintf ctx.verbose "Build deps succeeded.\n%!";
        let* () =
          run_build_step
            (Opam_build.build ~switch ~dir:ctx.dir ~package:ctx.package)
        in
        vprintf ctx.verbose "Build project succeeded.\n%!";
        let* () =
          run_build_step
            (Opam_build.test ~switch ~dir:ctx.dir ~package:ctx.package)
        in
        vprintf ctx.verbose "Test project succeeded.\n%!";
        Ok ()
      in
      let passed = match result with Ok _ -> true | Error _ -> false in
      vprintf ctx.verbose "OCaml %s %s.\n%!" version_string
        (if passed then "passed" else "failed");
      State.record ctx.state ~dep:"ocaml" ~ocaml_version:None
        ~version:version_string
        (if passed then `Pass else `Fail);
      if passed || save_on_fail then State.save ~dir:ctx.dir ctx.state;
      passed
    end

let array_find_last_index f arr =
  let len = Array.length arr in
  assert (len > 0);
  let rec loop i =
    assert (i >= 0);
    if f arr.(i) then i else loop (i - 1)
  in
  loop (len - 1)

let ( let>> ) = Option.bind

(* Binary-search an array of compiler versions for the lowest that passes.
   Return that value, as well as the maximum tested passing value. *)
let test_compiler ctx (versions : Version.t array) =
  let versions_map =
    Array.init (Array.length versions) (fun i ->
        let version = versions.(i) in
        let version_string = Version.to_string version in
        State.lookup ctx.state ~dep:"ocaml" ~ocaml_version:None
          ~version:version_string)
  in
  let min =
    binary_search versions versions_map 0
      (Array.length versions - 1)
      (test_compiler_version ctx)
  in
  let>> min = min in
  let i =
    array_find_last_index (function `Pass -> true | _ -> false) versions_map
  in
  let max_tested = versions.(i) in
  Some (min, max_tested)

(* A special path for OCaml 4 where it checks the highest version first. If
   that fails, the entire OCaml 4 range is skipped without further probing. *)
let test_ocaml4 ctx versions =
  let length = Array.length versions in
  let max_version = versions.(length - 1) in
  if test_compiler ctx [| max_version |] |> Option.is_none then None
  else
    let min_version =
      test_compiler ctx (Array.sub versions 0 (length - 1))
      |> Option.fold ~none:max_version ~some:fst
    in
    Some (min_version, max_version)

(* Do a binary search for the lowest passing version for the provided dep with
   the specified ocaml version, making sure that any bounds specific to that
   OCaml major version are adhered to. Pins the package to each tested version;
   unpins once after the search completes (via Fun.protect). *)
let test_dep ctx ocamlv switch_version (dep : Manifest.dep) all_dep_versions =
  let version_string = Version.to_string switch_version in
  let switch = Opam_switch.prefix ^ "ocaml-" ^ version_string in
  let versions =
    Manifest.filter_dep_versions ocamlv dep all_dep_versions |> Array.of_list
  in
  vprintf ctx.verbose "%s: %d version%s [%s]\n%!" dep.name
    (Array.length versions)
    (if Array.length versions = 1 then "" else "s")
    (Array.to_list versions |> List.map Version.to_string |> String.concat ", ");
  let ocaml_version =
    Some (match ocamlv with `Ocaml4 -> "ocaml4" | `Ocaml5 -> "ocaml5")
  in
  let versions_map =
    Array.init (Array.length versions) (fun i ->
        let version = versions.(i) in
        let version_string = Version.to_string version in
        State.lookup ctx.state ~dep:dep.name ~ocaml_version
          ~version:version_string)
  in
  let pinned = ref false in
  let switch_created = ref false in
  let test version =
    let version_string = Version.to_string version in
    let result =
      let* () =
        (* Lazily ensure the switch is created the first time we actually need
           it. This ensures that if we don't need it and [test] is never called,
           as we are running the program again a second time with -w, then we
           don't needlessly create the switch. *)
        if !switch_created then Ok ()
        else
          let* () =
            Opam_switch.find_or_create ~name:switch
              ~compiler:(Version.to_string switch_version)
          in
          switch_created := true;
          Ok ()
      in
      let* () =
        Opam_pin.add ~switch ~package:dep.name ~version:version_string
      in
      pinned := true;
      let* () = Opam_run.run_opam [ "upgrade"; "--switch"; switch; "--yes" ] in
      vprintf ctx.verbose "Created %s %s.\n%!" dep.name version_string;
      let* () = Opam_install.deps ~switch ~dir:ctx.dir in
      vprintf ctx.verbose "Build deps succeeded.\n%!";
      let* () =
        run_build_step
          (Opam_build.build ~switch ~dir:ctx.dir ~package:ctx.package)
      in
      vprintf ctx.verbose "Build project succeeded.\n%!";
      let* () =
        run_build_step
          (Opam_build.test ~switch ~dir:ctx.dir ~package:ctx.package)
      in
      vprintf ctx.verbose "Test project succeeded.\n%!";
      Ok ()
    in
    let passed = match result with Ok _ -> true | Error _ -> false in
    vprintf ctx.verbose "%s %s %s.\n%!" dep.name version_string
      (if passed then "passed" else "failed");
    State.record ctx.state ~dep:dep.name ~ocaml_version ~version:version_string
      (if passed then `Pass else `Fail);
    State.save ~dir:ctx.dir ctx.state;
    passed
  in
  let unpin_result = ref (Ok ()) in
  let result =
    Fun.protect
      (fun () ->
        binary_search versions versions_map 0 (Array.length versions - 1) test)
      ~finally:(fun () ->
        if !pinned then
          unpin_result := Opam_pin.remove ~switch ~package:dep.name)
  in
  match !unpin_result with
  | Error msg -> Error (sprintf "could not unpin %s: %s" dep.name msg)
  | Ok () -> Ok result
