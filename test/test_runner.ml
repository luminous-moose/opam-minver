open Opam_minver
open Manifest

let () = Process.am_test := true
let dep name bound : dep = { name; scope = Runtime; bound }
let versions = [ "0.9.0"; "1.0.0"; "1.1.0"; "1.2.0"; "2.0.0" ]

let filter ocamlv dep versions =
  List.map Version.of_string versions
  |> Manifest.filter_dep_versions ocamlv dep
  |> List.map Version.to_string

(* happy path *)

let%test "unconstrained returns all versions" =
  filter `Ocaml4 (dep "foo" (Simple Unconstrained)) versions = versions

let%test "At_least includes the bound version" =
  filter `Ocaml4 (dep "foo" (Simple (At_least "1.0.0"))) versions
  = [ "1.0.0"; "1.1.0"; "1.2.0"; "2.0.0" ]

let%test "At_least excludes versions below bound" =
  filter `Ocaml4 (dep "foo" (Simple (At_least "1.1.0"))) versions
  = [ "1.1.0"; "1.2.0"; "2.0.0" ]

let%test "At_least uses numerical comparison" =
  let vs = [ "4.8.0"; "4.9.0"; "4.10.0"; "4.11.0" ] in
  filter `Ocaml4 (dep "ocaml" (Simple (At_least "4.10.0"))) vs
  = [ "4.10.0"; "4.11.0" ]

(* edge cases *)

let%test "Below excludes versions at and above bound" =
  filter `Ocaml4
    (dep "ocaml" (Simple (Below "5.0.0")))
    (versions @ [ "5.0.0"; "5.1.0" ])
  = versions

let%test "Bounded includes only the range" =
  let vs = [ "4.08.0"; "4.14.0"; "4.14.1"; "5.0.0"; "5.1.0" ] in
  filter `Ocaml4 (dep "ocaml" (Simple (Bounded ("4.14.0", "5.0.0")))) vs
  = [ "4.14.0"; "4.14.1" ]

let%test "Skip returns empty list" =
  filter `Ocaml4 (dep "foo" (Skip "")) versions = []

let%test "Ocaml_split uses ocaml4 bound for Ocaml4" =
  let d =
    dep "foo"
      (Ocaml_split { ocaml4 = At_least "1.0.0"; ocaml5 = At_least "1.2.0" })
  in
  filter `Ocaml4 d versions = [ "1.0.0"; "1.1.0"; "1.2.0"; "2.0.0" ]

let%test "Ocaml_split uses ocaml5 bound for Ocaml5" =
  let d =
    dep "foo"
      (Ocaml_split { ocaml4 = At_least "1.0.0"; ocaml5 = At_least "1.2.0" })
  in
  filter `Ocaml5 d versions = [ "1.2.0"; "2.0.0" ]

let%test "Ocaml_split unconstrained side returns all" =
  let d =
    dep "foo"
      (Ocaml_split { ocaml4 = Unconstrained; ocaml5 = At_least "1.2.0" })
  in
  filter `Ocaml4 d versions = versions

(* expect test *)

let%expect_test "filter_versions" =
  let show label d ocamlv =
    let result = filter ocamlv d versions in
    Printf.printf "%-20s [%s]\n" label (String.concat "; " result)
  in
  show "unconstrained" (dep "foo" (Simple Unconstrained)) `Ocaml4;
  show "at_least 1.0" (dep "foo" (Simple (At_least "1.0.0"))) `Ocaml4;
  show "at_least 1.1" (dep "foo" (Simple (At_least "1.1.0"))) `Ocaml4;
  show "below 1.1" (dep "foo" (Simple (Below "1.1.0"))) `Ocaml4;
  show "skip" (dep "foo" (Skip "")) `Ocaml4;
  show "split/ocaml4"
    (dep "foo"
       (Ocaml_split { ocaml4 = At_least "1.0.0"; ocaml5 = At_least "1.2.0" }))
    `Ocaml4;
  show "split/ocaml5"
    (dep "foo"
       (Ocaml_split { ocaml4 = At_least "1.0.0"; ocaml5 = At_least "1.2.0" }))
    `Ocaml5;
  [%expect
    {|
    unconstrained        [0.9.0; 1.0.0; 1.1.0; 1.2.0; 2.0.0]
    at_least 1.0         [1.0.0; 1.1.0; 1.2.0; 2.0.0]
    at_least 1.1         [1.1.0; 1.2.0; 2.0.0]
    below 1.1            [0.9.0; 1.0.0]
    skip                 []
    split/ocaml4         [1.0.0; 1.1.0; 1.2.0; 2.0.0]
    split/ocaml5         [1.2.0; 2.0.0]
    |}]

(* split_deps *)

let split_names ds =
  let ocamldep, rest = Result.get_ok (Manifest.split_deps ds) in
  ( Option.map (fun (d : dep) -> d.name) ocamldep,
    List.map (fun (d : dep) -> d.name) rest )

let%test "split_deps empty list" = split_names [] = (None, [])

let%test "split_deps no ocaml dep returns all" =
  split_names
    [ dep "foo" (Simple Unconstrained); dep "bar" (Simple Unconstrained) ]
  = (None, [ "foo"; "bar" ])

let%test "split_deps extracts ocaml dep" =
  split_names
    [
      dep "ocaml" (Simple (At_least "4.14.0")); dep "foo" (Simple Unconstrained);
    ]
  = (Some "ocaml", [ "foo" ])

let%test "split_deps ocaml at end preserves rest order" =
  split_names
    [
      dep "foo" (Simple Unconstrained);
      dep "bar" (Simple Unconstrained);
      dep "ocaml" (Simple Unconstrained);
    ]
  = (Some "ocaml", [ "foo"; "bar" ])

let%expect_test "split_deps" =
  let show label ds =
    match Manifest.split_deps ds with
    | Error msg -> Printf.printf "%s: Error: %s\n" label msg
    | Ok (ocamldep, rest) ->
        Printf.printf "%s: ocaml=%s rest=[%s]\n" label
          (match ocamldep with None -> "none" | Some d -> d.name)
          (String.concat "; " (List.map (fun (d : dep) -> d.name) rest))
  in
  show "empty" [];
  show "no-ocaml"
    [ dep "foo" (Simple Unconstrained); dep "bar" (Simple Unconstrained) ];
  show "ocaml-first"
    [ dep "ocaml" (Simple Unconstrained); dep "foo" (Simple Unconstrained) ];
  show "ocaml-last"
    [ dep "foo" (Simple Unconstrained); dep "ocaml" (Simple Unconstrained) ];
  show "ocaml-middle"
    [
      dep "foo" (Simple Unconstrained);
      dep "ocaml" (Simple Unconstrained);
      dep "bar" (Simple Unconstrained);
    ];
  show "duplicate-ocaml"
    [
      dep "ocaml" (Simple Unconstrained);
      dep "foo" (Simple Unconstrained);
      dep "ocaml" (Simple (At_least "4.14.0"));
    ];
  [%expect
    {|
    empty: ocaml=none rest=[]
    no-ocaml: ocaml=none rest=[foo; bar]
    ocaml-first: ocaml=ocaml rest=[foo]
    ocaml-last: ocaml=ocaml rest=[foo]
    ocaml-middle: ocaml=ocaml rest=[foo; bar]
    duplicate-ocaml: Error: ocaml listed as a dep more than once
    |}]

(* filter_ocaml_versions *)

let all_ocaml_versions =
  List.map Version.of_string
    [ "4.08.0"; "4.12.0"; "4.13.0"; "4.14.0"; "5.0.0"; "5.1.0"; "5.2.0" ]

let show_partition (v4, v5) =
  Printf.printf "4: [%s]\n5+: [%s]\n"
    (String.concat "; " (List.map Version.to_string v4))
    (String.concat "; " (List.map Version.to_string v5))

let v = Version.of_string

let%expect_test "filter_ocaml_versions no constraint" =
  Runner.filter_ocaml_versions None all_ocaml_versions (v "5.2.0")
  |> Result.get_ok |> show_partition;
  [%expect
    {|
    4: [4.08.0; 4.12.0; 4.13.0; 4.14.0]
    5+: [5.0.0; 5.1.0; 5.2.0]
    |}]

let%expect_test "filter_ocaml_versions simple at_least" =
  Runner.filter_ocaml_versions
    (Some (dep "ocaml" (Simple (At_least "4.14.0"))))
    all_ocaml_versions (v "5.2.0")
  |> Result.get_ok |> show_partition;
  [%expect {|
    4: [4.14.0]
    5+: [5.0.0; 5.1.0; 5.2.0]
    |}]

let%expect_test "filter_ocaml_versions simple below 5.0.0" =
  Runner.filter_ocaml_versions
    (Some (dep "ocaml" (Simple (Below "5.0.0"))))
    all_ocaml_versions (v "5.2.0")
  |> Result.get_ok |> show_partition;
  [%expect {|
    4: [4.08.0; 4.12.0; 4.13.0; 4.14.0]
    5+: []
    |}]

let%expect_test "filter_ocaml_versions ocaml_split union and dedup" =
  (* ocaml4 filter includes 5.x that beat the 4.12 floor; ocaml5 filter is
     narrower. sort_and_filter deduplicates the overlap so each version appears
     once, then we partition by major. *)
  Runner.filter_ocaml_versions
    (Some
       (dep "ocaml"
          (Ocaml_split { ocaml4 = At_least "4.12.0"; ocaml5 = At_least "5.1.0" })))
    all_ocaml_versions (v "5.2.0")
  |> Result.get_ok |> show_partition;
  [%expect
    {|
    4: [4.12.0; 4.13.0; 4.14.0]
    5+: [5.0.0; 5.1.0; 5.2.0]
    |}]

let%expect_test
    "filter_ocaml_versions capped at ocaml4 version does not exclude ocaml5" =
  Runner.filter_ocaml_versions None all_ocaml_versions (v "4.14.0")
  |> Result.get_ok |> show_partition;
  [%expect
    {|
    4: [4.08.0; 4.12.0; 4.13.0; 4.14.0]
    5+: [5.0.0; 5.1.0; 5.2.0]
    |}]

let%expect_test "filter_ocaml_versions capped at ocaml5 mid-range" =
  Runner.filter_ocaml_versions None all_ocaml_versions (v "5.1.0")
  |> Result.get_ok |> show_partition;
  [%expect
    {|
    4: [4.08.0; 4.12.0; 4.13.0; 4.14.0]
    5+: [5.0.0; 5.1.0]
    |}]

let%expect_test "filter_ocaml_versions skip bound returns error" =
  (match
     Runner.filter_ocaml_versions
       (Some (dep "ocaml" (Skip {|"ocaml" {os = "linux"}|})))
       all_ocaml_versions (v "5.2.0")
   with
  | Ok _ -> print_string "unexpected ok"
  | Error msg -> print_string msg);
  [%expect
    {| ocaml dependency has an unrecognised constraint; cannot determine OCaml version bounds |}]

(* binary_search *)

let mk_test vs_strs bool_list =
  let tbl = Hashtbl.create 8 in
  List.iter2 (fun s b -> Hashtbl.add tbl s b) vs_strs bool_list;
  fun v -> Hashtbl.find tbl (Version.to_string v)

let binary_search vs_strs bool_list =
  let vs = Array.of_list (List.map Version.of_string vs_strs) in
  let map = Array.make (Array.length vs) `Unknown in
  let test = mk_test vs_strs bool_list in
  Probe.binary_search vs map 0 (Array.length vs - 1) test
  |> Option.map Version.to_string

let vs5 = [ "1.0.0"; "2.0.0"; "3.0.0"; "4.0.0"; "5.0.0" ]

let%test "binary_search all work returns first" =
  binary_search vs5 [ true; true; true; true; true ] = Some "1.0.0"

let%test "binary_search none work returns None" =
  binary_search vs5 [ false; false; false; false; false ] = None

let%test "binary_search only last version works" =
  binary_search vs5 [ false; false; false; false; true ] = Some "5.0.0"

let%test "binary_search floor in the middle" =
  binary_search vs5 [ false; false; true; true; true ] = Some "3.0.0"

let%test "binary_search single element works" =
  binary_search [ "1.0.0" ] [ true ] = Some "1.0.0"

let%test "binary_search single element fails" =
  binary_search [ "1.0.0" ] [ false ] = None

let%test "binary_search empty range returns None" =
  let vs = Array.of_list (List.map Version.of_string vs5) in
  let map = Array.make 5 `Unknown in
  let test _ = assert false in
  Probe.binary_search vs map 3 2 test = None

let%test "binary_search precomputed Pass skips test call" =
  let vs = Array.of_list (List.map Version.of_string vs5) in
  let map = Array.make 5 `Pass in
  let test _ = assert false in
  Probe.binary_search vs map 0 4 test = Some (Version.of_string "1.0.0")

let%test "binary_search precomputed Fail skips test call" =
  let vs = Array.of_list (List.map Version.of_string vs5) in
  let map = Array.make 5 `Fail in
  let test _ = assert false in
  Probe.binary_search vs map 0 4 test = None

(* merge_results *)

let%test "merge_results: dep absent from both result lists is unchanged" =
  let d = dep "foo" (Simple (At_least "1.0")) in
  match Runner.merge_results [ d ] None None with
  | [ r ] -> dep_equal r d
  | _ -> false

let%test "merge_results: dep in o4 only gets simple lower bound" =
  let d = dep "foo" (Simple Unconstrained) in
  match Runner.merge_results [ d ] (Some [ (d, v "1.1.0") ]) None with
  | [ r ] -> dep_bound_equal r.bound (Simple (At_least "1.1.0"))
  | _ -> false

let%test "merge_results: dep in o5 only gets simple lower bound" =
  let d = dep "foo" (Simple Unconstrained) in
  match Runner.merge_results [ d ] None (Some [ (d, v "2.0.0") ]) with
  | [ r ] -> dep_bound_equal r.bound (Simple (At_least "2.0.0"))
  | _ -> false

let%test "merge_results: equal o4 and o5 floors collapse to Simple" =
  let d = dep "foo" (Simple Unconstrained) in
  let o4 = Some [ (d, v "1.1.0") ] in
  let o5 = Some [ (d, v "1.1.0") ] in
  match Runner.merge_results [ d ] o4 o5 with
  | [ r ] -> dep_bound_equal r.bound (Simple (At_least "1.1.0"))
  | _ -> false

let%test "merge_results: different o4 and o5 floors produce Ocaml_split" =
  let d = dep "foo" (Simple Unconstrained) in
  let o4 = Some [ (d, v "1.1.0") ] in
  let o5 = Some [ (d, v "1.2.0") ] in
  match Runner.merge_results [ d ] o4 o5 with
  | [ r ] ->
      dep_bound_equal r.bound
        (Ocaml_split { ocaml4 = At_least "1.1.0"; ocaml5 = At_least "1.2.0" })
  | _ -> false

let%test "merge_results: Skip dep unchanged regardless of result entries" =
  let s = {|"foo" {>= "1.0" & os = "linux"}|} in
  let d = dep "foo" (Skip s) in
  let o4 = Some [ (d, v "2.0.0") ] in
  let o5 = Some [ (d, v "3.0.0") ] in
  match Runner.merge_results [ d ] o4 o5 with
  | [ r ] -> dep_bound_equal r.bound (Skip s)
  | _ -> false

let%expect_test "merge_results: multiple deps with varied outcomes" =
  let unchanged = dep "unchanged" (Simple (At_least "0.5")) in
  let o4only = dep "o4only" (Simple Unconstrained) in
  let o5only = dep "o5only" (Simple Unconstrained) in
  let both_same = dep "same" (Simple Unconstrained) in
  let both_diff = dep "diff" (Simple Unconstrained) in
  let deps = [ unchanged; o4only; o5only; both_same; both_diff ] in
  let o4_results =
    Some [ (o4only, v "1.0.0"); (both_same, v "1.1.0"); (both_diff, v "1.0.0") ]
  in
  let o5_results =
    Some [ (o5only, v "2.0.0"); (both_same, v "1.1.0"); (both_diff, v "1.2.0") ]
  in
  Runner.merge_results deps o4_results o5_results
  |> List.iter (fun d -> print_endline (dep_to_string d));
  [%expect
    {|
    "unchanged" {>= "0.5"}
    "o4only" {>= "1.0.0"}
    "o5only" {>= "2.0.0"}
    "same" {>= "1.1.0"}
    "diff" {>= "1.2.0"}
    |}]

(* run_build_step *)

let%test "run_build_step Ok passes through" =
  Probe.run_build_step (Ok ()) = Ok ()

let%test "run_build_step Build_failure becomes Error with message" =
  Probe.run_build_step (Error (Opam_build.Build_failure "oops")) = Error "oops"

let%test "run_build_step Missing_libraries raises with libs" =
  match
    Probe.run_build_step (Error (Opam_build.Missing_libraries [ "foo"; "bar" ]))
  with
  | _ -> false
  | exception Probe.Missing_libraries libs -> libs = [ "foo"; "bar" ]
  | exception _ -> false

let%test "run_build_step Missing_libraries empty list" =
  match Probe.run_build_step (Error (Opam_build.Missing_libraries [])) with
  | _ -> false
  | exception Probe.Missing_libraries libs -> libs = []
  | exception _ -> false

let%expect_test "binary_search trace" =
  let vs = Array.of_list (List.map Version.of_string vs5) in
  let map = Array.make 5 `Unknown in
  let bool_arr = [| false; false; true; true; true |] in
  let tested = ref [] in
  let test v =
    let s = Version.to_string v in
    tested := s :: !tested;
    let idx = ref 0 in
    Array.iteri (fun i u -> if Version.to_string u = s then idx := i) vs;
    bool_arr.(!idx)
  in
  let result = Probe.binary_search vs map 0 4 test in
  Printf.printf "result: %s\n"
    (match result with None -> "none" | Some v -> Version.to_string v);
  Printf.printf "tested (in order): [%s]\n"
    (String.concat "; " (List.rev !tested));
  [%expect
    {|
    result: 3.0.0
    tested (in order): [3.0.0; 1.0.0; 2.0.0]
    |}]

let mk_trace_test vs_strs bool_arr =
  let vs = Array.of_list (List.map Version.of_string vs_strs) in
  let map = Array.make (Array.length vs) `Unknown in
  let tested = ref [] in
  let test v =
    let s = Version.to_string v in
    tested := s :: !tested;
    let idx = ref 0 in
    Array.iteri (fun i u -> if Version.to_string u = s then idx := i) vs;
    bool_arr.(!idx)
  in
  let result =
    Probe.binary_search vs map 0 (Array.length vs - 1) test
    |> Option.map Version.to_string
  in
  (result, List.rev !tested)

let%expect_test "binary_search trace: unconstrained, floor at index 1" =
  (* Five versions; 1.0.0 fails, rest pass.  Probes: mid=2 (1.2.0 passes),
     then left to mid=0 (1.0.0 fails), then mid=1 (1.1.0 passes). *)
  let result, tested =
    mk_trace_test
      [ "1.0.0"; "1.1.0"; "1.2.0"; "1.3.0"; "1.4.0" ]
      [| false; true; true; true; true |]
  in
  Printf.printf "result: %s\n" (Option.value result ~default:"none");
  Printf.printf "tested: [%s]\n" (String.concat "; " tested);
  [%expect {|
    result: 1.1.0
    tested: [1.2.0; 1.0.0; 1.1.0]
    |}]

let%expect_test "binary_search trace: all pass, list starts at bound version" =
  (* Simulates what test_dep receives after filter_dep_versions(At_least "1.1.0"):
     [1.1.0; 1.2.0; 1.3.0; 1.4.0].  Without the optimization binary_search starts
     at mid=1 (1.2.0), not at index 0 (the bound version 1.1.0).  After the
     optimization, test_dep probes 1.1.0 first and skips binary_search entirely. *)
  let result, tested =
    mk_trace_test
      [ "1.1.0"; "1.2.0"; "1.3.0"; "1.4.0" ]
      [| true; true; true; true |]
  in
  Printf.printf "result: %s\n" (Option.value result ~default:"none");
  Printf.printf "tested: [%s]\n" (String.concat "; " tested);
  [%expect {|
    result: 1.1.0
    tested: [1.2.0; 1.1.0]
    |}]
