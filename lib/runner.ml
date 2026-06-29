open Printf
open Result_syntax

type probe_env = {
  ocaml_versions : unit -> (Version.t list, string) result;
  current_ocaml : unit -> (Version.t, string) result;
  dep_versions : string -> (Opam_versions.version_type, string) result;
  remove_all_pins : string -> (unit, string) result;
  run_combined_validation :
    package:string ->
    dir:string ->
    verbose:bool ->
    [ `Ocaml4 | `Ocaml5 ] ->
    Version.t ->
    (Manifest.dep * Version.t) list ->
    (unit, string) result;
}

(* Use the provided ocamldep as a filter, if present, to produce a list of OCaml
   4 versions and a list of OCaml 5 versions to test. Within the major version
   of OCaml that is the current version, do not test higher versions than that.
   The reason for that is that frequently, dependencies that involve derivers
   might fail on the latest compiler version but only because they haven't been
   updated. It is assumed that the current compiler version works and this
   avoids those spurious failures. *)
let filter_ocaml_versions (ocamldep : Manifest.dep option) ocamlversions_all
    current_ocaml_version =
  let* ocamlversions =
    match ocamldep with
    | None -> Ok ocamlversions_all
    | Some dep -> (
        match dep.bound with
        | Simple s ->
            Ok
              (Manifest.apply_filter s ocamlversions_all
              |> Version.sort_and_filter)
        | Ocaml_split { ocaml4; ocaml5 } ->
            Ok
              (List.concat_map
                 (fun f -> Manifest.apply_filter f ocamlversions_all)
                 [ ocaml4; ocaml5 ]
              |> Version.sort_and_filter)
        | Skip _ ->
            Error
              "ocaml dependency has an unrecognised constraint; cannot \
               determine OCaml version bounds")
  in
  Ok
    (List.filter (fun v -> Version.major v >= 4) ocamlversions
    |> List.filter (fun v ->
        Version.major v <> Version.major current_ocaml_version
        || Version.compare v current_ocaml_version <= 0)
    |> List.partition (fun v -> Version.major v = 4))

let print_result label ineq v =
  printf "%s: %s %s\n%!" label ineq (Version.to_string v)

let print_split_note new_deps =
  let splits = Manifest.split_report new_deps in
  if splits <> [] then begin
    print_endline
      "Note: different minimum versions found for OCaml 4 and OCaml 5.";
    print_endline
      "opam cannot express per-major-version bounds; the higher minimum is \
       used.";
    List.iter
      (fun (name, o4, o5) ->
        let bs sb =
          match Manifest.simple_bound_to_string sb with "" -> "any" | s -> s
        in
        printf "  %s: OCaml 4 %s, OCaml 5 %s\n%!" name (bs o4) (bs o5))
      splits
  end

type ocaml_probe_result = {
  ocaml4_min_and_test : (Version.t * Version.t) option;
  ocaml5_min_and_test : (Version.t * Version.t) option;
}

(* Conduct the probes of OCaml 4 and OCaml 5 versions and return an
   ocaml_probe_result. *)
let probe_ocaml_versions env ctx ocamldep =
  let* ocamlversions_all = env.ocaml_versions () in
  let* current = env.current_ocaml () in
  try
    let current_passed =
      Probe.test_compiler_version ~save_on_fail:false ctx current
    in
    if not current_passed then
      Error
        "project does not build and pass tests with current compiler version"
    else
      let* ocaml4, ocaml5 =
        filter_ocaml_versions ocamldep ocamlversions_all current
      in
      let ocaml4_arr = Array.of_list ocaml4 in
      let ocaml5_arr = Array.of_list ocaml5 in

      let ocaml4_min_and_max_tested =
        if Array.length ocaml4_arr = 0 then None
        else begin
          if ctx.verbose then
            printf "ocaml (4): %d version%s [%s]\n%!" (Array.length ocaml4_arr)
              (if Array.length ocaml4_arr = 1 then "" else "s")
              (Array.to_list ocaml4_arr |> List.map Version.to_string
             |> String.concat ", ");
          Probe.test_ocaml4 ctx ocaml4_arr
        end
      in
      let ocaml5_min_and_max_tested =
        if Array.length ocaml5_arr = 0 then None
        else begin
          if ctx.verbose then
            printf "ocaml (5): %d version%s [%s]\n%!" (Array.length ocaml5_arr)
              (if Array.length ocaml5_arr = 1 then "" else "s")
              (Array.to_list ocaml5_arr |> List.map Version.to_string
             |> String.concat ", ");
          Probe.test_compiler ctx ocaml5_arr
        end
      in
      match (ocaml4_min_and_max_tested, ocaml5_min_and_max_tested) with
      | None, None -> Error "No OCaml compilers pass"
      | ( (Some (o4min, _) as ocaml4_min_and_test),
          (Some (o5min, _) as ocaml5_min_and_test) ) ->
          let major = Version.major current in
          if major = 4 then
            Ok
              {
                ocaml4_min_and_test = Some (o4min, current);
                ocaml5_min_and_test;
              }
          else
            Ok
              {
                ocaml4_min_and_test;
                ocaml5_min_and_test = Some (o5min, current);
              }
      | ocaml4_min_and_test, ocaml5_min_and_test ->
          Ok { ocaml4_min_and_test; ocaml5_min_and_test }
  with Probe.Missing_libraries libs ->
    Error
      (sprintf "libraries used in dune files but not declared in .opam: %s"
         (String.concat ", " libs))

(* Fold a list with a function that produces results into a final result. *)
let fold_result l ~init ~f : ('accum, 'e) result =
  let rec loop acc l =
    match l with
    | [] -> Ok acc
    | hd :: tl ->
        begin match f acc hd with Ok acc -> loop acc tl | Error e -> Error e
        end
  in
  loop init l

(* Probe the minimum version of each dependency in the switch for OCaml version
   otest. For speed, they are all probed within that switch by pinning their
   versions one at a time, and then removing the pin. This is usually a much
   faster process than testing the compiler versions and avoids a combinatorial
   explosion of versions to test. The independence of min versions of each dep
   is assumed: any interdependencies can't be found by this program nor are they
   practical to find in general. *)
let probe_deps env ctx ocamlv otest deps =
  let switch = Opam_switch.prefix ^ "ocaml-" ^ Version.to_string otest in
  let filtered =
    List.filter
      (fun dep ->
        match dep.Manifest.bound with
        | Skip _ -> false
        | _ -> ( match dep.scope with With_doc -> false | _ -> true))
      deps
  in
  if filtered = [] then Ok []
  else
    let* () = env.remove_all_pins switch in
    filtered
    |> fold_result ~init:[] ~f:(fun acc dep ->
        let* all_dep_versions = env.dep_versions dep.Manifest.name in
        match all_dep_versions with
        | Contains_base -> Ok acc
        | Versions all_dep_versions -> (
            let* result =
              Probe.test_dep ctx ocamlv otest dep all_dep_versions
            in
            match result with
            | Some result -> Ok ((dep, result) :: acc)
            | None -> Error (sprintf "no version found for %s" dep.name)))

(* Given the original list of deps and the ocaml4 and ocaml5 results, merge
   them into a final list of deps to output. *)
let merge_results (deps : Manifest.dep list) o4results o5results =
  let find name = function
    | None -> None
    | Some lst ->
        List.find_opt (fun (dep, _) -> String.equal dep.Manifest.name name) lst
        |> Option.map (fun (_, v) -> Version.to_string v)
  in
  List.map
    (fun dep ->
      let o4v = find dep.Manifest.name o4results in
      let o5v = find dep.Manifest.name o5results in
      (* OCaml must be handled specially, because if we have OCaml 4 results
         but no 5, then it is always correct, and mandatory, to limit its bound
         to OCaml 4, because the only way we can have no OCaml 5 results is
         either a preexisting bound prohibiting it, or all tested versions
         failed. *)
      match (dep.name, o4v, o5v) with
      | "ocaml", Some v, None ->
          { dep with bound = Manifest.Simple (Manifest.Bounded (v, "5.0.0")) }
      (* 5.0.0 is the earliest possible OCaml 5 version, so if that passes, no
         upper bound is needed at all. *)
      | "ocaml", Some o4v, Some o5v when String.equal o5v "5.0.0" ->
          { dep with bound = Manifest.Simple (Manifest.At_least o4v) }
      | _, None, None -> dep (* handles Skips *)
      | _ ->
          let bound = Manifest.merge_dep_bound dep.Manifest.bound o4v o5v in
          { dep with bound })
    deps

(* Run the project's build and tests in [switch], converting Opam_build errors
   to string results. *)
let build_and_test ~switch ~dir ~package =
  let to_result = function
    | Ok () -> Ok ()
    | Error (Opam_build.Missing_libraries libs) ->
        Error (sprintf "missing libraries: %s" (String.concat ", " libs))
    | Error (Opam_build.Build_failure msg) -> Error msg
  in
  let* () = to_result (Opam_build.build ~switch ~dir ~package) in
  to_result (Opam_build.test ~switch ~dir ~package)

(* Pin all deps to their discovered minimums in [switch] and run a combined
   build+test. Returns Ok () regardless of build outcome; failures are printed
   as warnings so the caller still gets to write the bounds it found. *)
let validate_combined ~package ~dir ~verbose ocamlv ocaml_min dep_results =
  let suffix = match ocamlv with `Ocaml4 -> "ocaml4" | `Ocaml5 -> "ocaml5" in
  let switch = Opam_switch.prefix ^ "combined-" ^ suffix in
  let major = Version.major ocaml_min in
  let pins =
    List.filter_map
      (fun (dep, version) ->
        match dep.Manifest.name with
        | "ocaml" -> None
        | _ -> Some (dep.Manifest.name, Version.to_string version))
      dep_results
  in
  if verbose then begin
    printf "Combined validation (OCaml %d):\n%!" major;
    List.iter (fun (n, v) -> printf "  pinning %s = %s\n%!" n v) pins
  end;
  let result =
    let* () =
      Opam_switch.find_or_create ~name:switch
        ~compiler:(Version.to_string ocaml_min)
    in
    let* () = Opam_pin.remove_all ~switch in
    let* () =
      fold_result pins ~init:() ~f:(fun () (pkg, version) ->
          Opam_pin.add ~switch ~package:pkg ~version)
    in
    let* () = Opam_run.run_opam [ "upgrade"; "--switch"; switch; "--yes" ] in
    let* () = Opam_install.deps ~switch ~dir in
    build_and_test ~switch ~dir ~package
  in
  (match result with
  | Ok () -> printf "Combined validation (OCaml %d): PASS\n%!" major
  | Error msg -> printf "Combined validation (OCaml %d): FAIL: %s\n%!" major msg);
  Ok ()

let dep_fingerprint dep_results =
  dep_results
  |> List.map (fun (dep, version) ->
      dep.Manifest.name ^ "=" ^ Version.to_string version)
  |> List.sort String.compare |> String.concat ","

let maybe_validate_combined env state ~package ~dir ~verbose ocamlv
    ocaml_min_and_test dep_results =
  match (ocaml_min_and_test, dep_results) with
  | Some (ocaml_min, _), Some dep_results ->
      let ocaml_key =
        match ocamlv with `Ocaml4 -> "ocaml4" | `Ocaml5 -> "ocaml5"
      in
      let fingerprint = dep_fingerprint dep_results in
      if State.combined_done state ocaml_key fingerprint then begin
        let major = match ocamlv with `Ocaml4 -> 4 | `Ocaml5 -> 5 in
        printf "Combined validation (OCaml %d): already done, skipping.\n%!"
          major;
        Ok ()
      end
      else begin
        let* () =
          env.run_combined_validation ~package ~dir ~verbose ocamlv ocaml_min
            dep_results
        in
        State.record_combined state ocaml_key fingerprint;
        Ok ()
      end
  | _ -> Ok ()

(* Compute all bounds for both OCaml versions and dep versions. *)
let compute_bounds env state dir ~verbose ~package (deps : Manifest.dep list) =
  let ctx : Probe.ctx = { state; dir; package; verbose } in
  let* ocamldep, rest = Manifest.split_deps deps in
  let* ocaml_result = probe_ocaml_versions env ctx ocamldep in
  (* OCaml might not be specified as a dep: handle that case by injecting it
     as one if so. *)
  let ocamldep, deps =
    match ocamldep with
    | Some ocamldep -> (ocamldep, deps)
    | None ->
        let ocamldep =
          {
            Manifest.name = "ocaml";
            scope = Runtime;
            bound = Simple Unconstrained;
          }
        in
        (ocamldep, ocamldep :: deps)
  in
  (* Process each OCaml major version separately. *)
  let probe_compiler_version_deps ocamlv ocaml_version_min_and_test =
    let version_num = match ocamlv with `Ocaml4 -> 4 | `Ocaml5 -> 5 in
    match ocaml_version_min_and_test with
    | Some (omin, otest) ->
        if verbose then begin
          print_result (sprintf "ocaml (%d)" version_num) ">=" omin;
          print_result (sprintf "ocaml (%d) test version" version_num) "=" otest
        end;
        let* results = probe_deps env ctx ocamlv otest rest in
        let results = List.rev results in
        let results = (ocamldep, omin) :: results in
        Ok (Some results)
    | None ->
        if verbose then
          printf "ocaml (%d): no compatible version found\n%!" version_num;
        Ok None
  in
  let* o4_dep_results =
    probe_compiler_version_deps `Ocaml4 ocaml_result.ocaml4_min_and_test
  in
  let* o5_dep_results =
    probe_compiler_version_deps `Ocaml5 ocaml_result.ocaml5_min_and_test
  in
  let* () =
    maybe_validate_combined env state ~package ~dir ~verbose `Ocaml4
      ocaml_result.ocaml4_min_and_test o4_dep_results
  in
  let* () =
    maybe_validate_combined env state ~package ~dir ~verbose `Ocaml5
      ocaml_result.ocaml5_min_and_test o5_dep_results
  in
  Ok (merge_results deps o4_dep_results o5_dep_results)

(* Top level runner: compute all bounds, and either write out the results to the
   opam file, or print them to stdout. env is injectable so that the function
   can be tested without real opam side-effects. *)
let run_with env state dir ?preamble (manifest : Manifest.t) ~verbose ~write_out
    =
  let package = Filename.basename manifest.path |> Filename.chop_extension in
  let* new_deps =
    compute_bounds env state dir ~verbose ~package manifest.parsed_deps
  in
  State.save ~dir state;
  if Manifest.dep_section_lines new_deps = Manifest.original_dep_lines manifest
  then begin
    print_endline "No change to dependency bounds.";
    Ok ()
  end
  else if write_out then begin
    print_split_note new_deps;
    let* () = Manifest.write_out manifest new_deps in
    print_endline "Opam file written.";
    Ok ()
  end
  else begin
    print_split_note new_deps;
    Option.iter print_endline preamble;
    print_endline "Dep bounds found:";
    let dep_lines = Manifest.dep_section_lines new_deps in
    List.iter print_endline dep_lines;
    Ok ()
  end

let real_env =
  {
    ocaml_versions = Opam_versions.available_compilers;
    current_ocaml = Opam_switch.current_ocaml_version;
    dep_versions = (fun pkg -> Opam_versions.available ~package:pkg);
    remove_all_pins = (fun switch -> Opam_pin.remove_all ~switch);
    run_combined_validation = validate_combined;
  }

(* Top level runner for main. *)
let run state dir manifest ~preamble ~verbose ~write_out =
  run_with real_env state dir manifest ?preamble ~verbose ~write_out
