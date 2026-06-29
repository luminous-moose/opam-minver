open Opam_minver.Manifest

let () = Opam_minver.Process.am_test := true
let read = Opam_minver.Manifest_parse.read

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
  let path = Filename.temp_file "opam_minver_test_manifest_" ".opam" in
  let oc = open_out path in
  output_string oc content;
  close_out oc;
  Fun.protect ~finally:(fun () -> Sys.remove path) (fun () -> f path)

let read_deps deps_str =
  with_opam_file deps_str @@ fun path ->
  match read path with
  | Ok t -> deps t
  | Error msg -> failwith ("read failed: " ^ msg)

(* --- inline tests --- *)

let%test "bare dep: unconstrained runtime" =
  match read_deps {|  "cmdliner"|} with
  | [ { name = "cmdliner"; scope = Runtime; bound = Simple Unconstrained } ] ->
      true
  | _ -> false

let%test "simple lower bound" =
  match read_deps {|  "cmdliner" {>= "1.1.0"}|} with
  | [ { scope = Runtime; bound = Simple (At_least "1.1.0"); _ } ] -> true
  | _ -> false

let%test "upper bound only" =
  match read_deps {|  "ocaml" {< "5.0.0"}|} with
  | [ { scope = Runtime; bound = Simple (Below "5.0.0"); _ } ] -> true
  | _ -> false

let%test "bounded range" =
  match read_deps {|  "ocaml" {>= "4.14.0" & < "5.0.0"}|} with
  | [ { scope = Runtime; bound = Simple (Bounded ("4.14.0", "5.0.0")); _ } ] ->
      true
  | _ -> false

let%test "with-test scope, no bound" =
  match read_deps {|  "alcotest" {with-test}|} with
  | [ { scope = With_test; bound = Simple Unconstrained; _ } ] -> true
  | _ -> false

let%test "with-doc scope, no bound" =
  match read_deps {|  "odoc" {with-doc}|} with
  | [ { scope = With_doc; bound = Simple Unconstrained; _ } ] -> true
  | _ -> false

let%test "with-test and lower bound" =
  match read_deps {|  "alcotest" {with-test & >= "1.6"}|} with
  | [ { scope = With_test; bound = Simple (At_least "1.6"); _ } ] -> true
  | _ -> false

let%test "with-test and bounded range" =
  match read_deps {|  "alcotest" {with-test & >= "1.0" & < "3.0"}|} with
  | [ { scope = With_test; bound = Simple (Bounded ("1.0", "3.0")); _ } ] ->
      true
  | _ -> false

let%test "ocaml-split with ocaml:version" =
  let s =
    {|  "pkg" {>= "1.1.0" & ocaml:version < "5.0.0" | >= "1.3.0" & ocaml:version >= "5.0.0"}|}
  in
  match read_deps s with
  | [
   {
     scope = Runtime;
     bound =
       Ocaml_split { ocaml4 = At_least "1.1.0"; ocaml5 = At_least "1.3.0" };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "ocaml-split branches order-independent" =
  let s =
    {|  "pkg" {>= "1.3.0" & ocaml:version >= "5.0.0" | >= "1.1.0" & ocaml:version < "5.0.0"}|}
  in
  match read_deps s with
  | [
   {
     scope = Runtime;
     bound =
       Ocaml_split { ocaml4 = At_least "1.1.0"; ocaml5 = At_least "1.3.0" };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "ocaml-split with explicit parens" =
  let s =
    {|  "pkg" {(>= "1.1.0" & ocaml:version < "5.0.0") | (>= "1.3.0" & ocaml:version >= "5.0.0")}|}
  in
  match read_deps s with
  | [
   {
     scope = Runtime;
     bound =
       Ocaml_split { ocaml4 = At_least "1.1.0"; ocaml5 = At_least "1.3.0" };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "ocaml package: split without ocaml:version" =
  let s = {|  "ocaml" {>= "4.02" & < "4.12" | >= "5.2.0"}|} in
  match read_deps s with
  | [
   {
     name = "ocaml";
     scope = Runtime;
     bound =
       Ocaml_split
         { ocaml4 = Bounded ("4.02", "4.12"); ocaml5 = At_least "5.2.0" };
   };
  ] ->
      true
  | _ -> false

let%test "ocaml package: split bare bounds forward order" =
  let s = {|  "ocaml" {>= "4.11.0" & < "5.0" | >= "5.1.0"}|} in
  match read_deps s with
  | [
   {
     name = "ocaml";
     scope = Runtime;
     bound =
       Ocaml_split
         { ocaml4 = Bounded ("4.11.0", "5.0"); ocaml5 = At_least "5.1.0" };
   };
  ] ->
      true
  | _ -> false

let%test "ocaml package: split bare bounds reversed order" =
  let s = {|  "ocaml" {>= "5.1.0" | >= "4.11.0" & < "5.0"}|} in
  match read_deps s with
  | [
   {
     name = "ocaml";
     scope = Runtime;
     bound =
       Ocaml_split
         { ocaml4 = Bounded ("4.11.0", "5.0"); ocaml5 = At_least "5.1.0" };
   };
  ] ->
      true
  | _ -> false

let%test "scope + ocaml-split" =
  let s =
    {|  "pkg" {(with-test & >= "1.0" & ocaml:version < "5.0") | (with-test & >= "2.0" & ocaml:version >= "5.0")}|}
  in
  match read_deps s with
  | [
   {
     scope = With_test;
     bound = Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "factored scope + ocaml-split" =
  let s =
    {|  "pkg" {with-test & (>= "1.0" & ocaml:version < "5.0" | >= "2.0" & ocaml:version >= "5.0")}|}
  in
  match read_deps s with
  | [
   {
     scope = With_test;
     bound = Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" };
     _;
   };
  ] ->
      true
  | _ -> false

let%test "unrecognised filter is preserved as Skip" =
  match read_deps {|  "foo" {>= "1.0" & os = "linux"}|} with
  | [ { name = "foo"; bound = Skip _; _ } ] -> true
  | _ -> false

let%test "dep order is preserved" =
  let names =
    List.map
      (fun d -> d.name)
      (read_deps
         {|  "ocaml"    {>= "4.14"}
  "dune"     {>= "3.0"}
  "cmdliner" {>= "1.1"}|})
  in
  names = [ "ocaml"; "dune"; "cmdliner" ]

let%test "dep_range: start is non-negative" =
  with_opam_file {|  "dune" {>= "3.0"}|} @@ fun path ->
  match read path with Ok t -> fst (dep_range t) >= 0 | Error _ -> false

let%test "dep_range: end >= start" =
  with_opam_file {|  "dune" {>= "3.0"}|} @@ fun path ->
  match read path with
  | Ok t ->
      let s, e = dep_range t in
      e >= s
  | Error _ -> false

let%test "patchable for a normal file" =
  with_opam_file {|  "dune" {>= "3.0"}|} @@ fun path ->
  match read path with Ok t -> patchable t | Error _ -> false

let file_lines path =
  let ic = open_in path in
  let lines = ref [] in
  (try
     while true do
       lines := input_line ic :: !lines
     done
   with End_of_file -> ());
  close_in ic;
  Array.of_list (List.rev !lines)

let%test "unrecognised top-level entry returns error" =
  with_opam_file {|  foo|} @@ fun path ->
  match read path with Error _ -> true | Ok _ -> false

let%test "unrecognised entry among valid ones still returns error" =
  with_opam_file {|  "cmdliner" {>= "1.1"}
  foo
  "dune" {>= "3.0"}|}
  @@ fun path -> match read path with Error _ -> true | Ok _ -> false

let%test "dep_range start is 0-based index of 'depends:' line" =
  with_opam_file {|  "dune" {>= "3.0"}|} @@ fun path ->
  match read path with
  | Error _ -> false
  | Ok t ->
      let lines = file_lines path in
      let start, _ = dep_range t in
      String.length lines.(start) >= 8
      && String.sub lines.(start) 0 8 = "depends:"

let%test "dep_range stop is exclusive: lines.(stop-1) is ']'" =
  with_opam_file {|  "dune" {>= "3.0"}|} @@ fun path ->
  match read path with
  | Error _ -> false
  | Ok t ->
      let lines = file_lines path in
      let _, stop = dep_range t in
      String.trim lines.(stop - 1) = "]"

let%test "dep_range stop tracks multi-dep block" =
  with_opam_file
    {|  "dune" {>= "3.0"}
  "cmdliner" {>= "1.1"}
  "fmt" {>= "0.8"}|}
  @@ fun path ->
  match read path with
  | Error _ -> false
  | Ok t ->
      let lines = file_lines path in
      let _, stop = dep_range t in
      String.trim lines.(stop - 1) = "]"

(* --- expect test --- *)

let show_bound = function
  | Simple Unconstrained -> "any"
  | Simple (At_least v) -> ">= " ^ v
  | Simple (Below v) -> "< " ^ v
  | Simple (Bounded (lo, hi)) -> ">= " ^ lo ^ " & < " ^ hi
  | Ocaml_split { ocaml4; ocaml5 } ->
      let sb = function
        | Unconstrained -> "any"
        | At_least v -> ">= " ^ v
        | Below v -> "< " ^ v
        | Bounded (lo, hi) -> ">= " ^ lo ^ " & < " ^ hi
      in
      Printf.sprintf "ocaml4:(%s)  ocaml5:(%s)" (sb ocaml4) (sb ocaml5)
  | Skip s -> "skip:" ^ s

let show_scope = function
  | Runtime -> ""
  | With_test -> "[test] "
  | With_doc -> "[doc]  "

let show_dep d =
  Printf.sprintf "%-16s %s%s" d.name (show_scope d.scope) (show_bound d.bound)

let%expect_test "all cases" =
  let ds =
    read_deps
      {|
  "bare"
  "lower"        {>= "1.0"}
  "upper"        {< "5.0"}
  "bounded"      {>= "1.0" & < "5.0"}
  "test-only"    {with-test}
  "doc-only"     {with-doc}
  "test-lower"   {with-test & >= "1.6"}
  "test-bounded" {with-test & >= "1.0" & < "3.0"}
  "split"        {>= "1.0" & ocaml:version < "5.0" | >= "2.0" & ocaml:version >= "5.0"}
  "split-parens" {(>= "1.0" & ocaml:version < "5.0") | (>= "2.0" & ocaml:version >= "5.0")}
  "ocaml"        {>= "4.02" & < "4.12" | >= "5.2.0"}
  "test-split"         {(with-test & >= "1.0" & ocaml:version < "5.0") | (with-test & >= "2.0" & ocaml:version >= "5.0")}
  "test-split-factored" {with-test & (>= "1.0" & ocaml:version < "5.0" | >= "2.0" & ocaml:version >= "5.0")}
  "skip-me"            {>= "1.0" & os = "linux"}
|}
  in
  List.iter (fun d -> print_endline (show_dep d)) ds;
  [%expect
    {|
    bare             any
    lower            >= 1.0
    upper            < 5.0
    bounded          >= 1.0 & < 5.0
    test-only        [test] any
    doc-only         [doc]  any
    test-lower       [test] >= 1.6
    test-bounded     [test] >= 1.0 & < 3.0
    split            ocaml4:(>= 1.0)  ocaml5:(>= 2.0)
    split-parens     ocaml4:(>= 1.0)  ocaml5:(>= 2.0)
    ocaml            ocaml4:(>= 4.02 & < 4.12)  ocaml5:(>= 5.2.0)
    test-split       [test] ocaml4:(>= 1.0)  ocaml5:(>= 2.0)
    test-split-factored [test] ocaml4:(>= 1.0)  ocaml5:(>= 2.0)
    skip-me          skip:"skip-me" {>= "1.0" & os = "linux"}
    |}]

(* --- serialisation unit tests --- *)

let%test "simple_bound_to_string: unconstrained" =
  simple_bound_to_string Unconstrained = ""

let%test "simple_bound_to_string: at_least" =
  simple_bound_to_string (At_least "1.0") = {|>= "1.0"|}

let%test "simple_bound_to_string: below" =
  simple_bound_to_string (Below "5.0") = {|< "5.0"|}

let%test "simple_bound_to_string: bounded" =
  simple_bound_to_string (Bounded ("1.0", "5.0")) = {|>= "1.0" & < "5.0"|}

let%test "dep_bound_to_string: ocaml-split collapses to higher bound" =
  dep_bound_to_string
    (Ocaml_split { ocaml4 = Unconstrained; ocaml5 = At_least "2.0" })
  = {|>= "2.0"|}

let%test "dep_bound_to_string: ocaml-split picks ocaml4 when higher" =
  dep_bound_to_string
    (Ocaml_split { ocaml4 = At_least "3.0"; ocaml5 = At_least "2.0" })
  = {|>= "3.0"|}

let%test "dep_scope_to_string: with-test unconstrained" =
  dep_scope_to_string With_test (Simple Unconstrained) = "with-test"

let%test "dep_scope_to_string: with-doc unconstrained" =
  dep_scope_to_string With_doc (Simple Unconstrained) = "with-doc"

let%expect_test
    "dep_scope_to_string: with-test + ocaml-split collapses to higher bound" =
  let split =
    Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" }
  in
  let s = dep_scope_to_string With_test split in
  print_endline s;
  [%expect {| with-test & >= "2.0" |}]

let%test "simplify_dep_bound: equal branches collapse to Simple" =
  simplify_dep_bound
    (Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "1.0" })
  = Simple (At_least "1.0")

let%test "simplify_dep_bound: unequal branches unchanged" =
  let split =
    Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" }
  in
  dep_bound_equal (simplify_dep_bound split) split

let%test "simplify_dep_bound: Simple is unchanged" =
  simplify_dep_bound (Simple (At_least "1.0")) = Simple (At_least "1.0")

let%test "dep_to_string: bare runtime dep" =
  dep_to_string
    { name = "cmdliner"; scope = Runtime; bound = Simple Unconstrained }
  = {|"cmdliner"|}

let%test "dep_to_string: with-test no bound" =
  dep_to_string
    { name = "alcotest"; scope = With_test; bound = Simple Unconstrained }
  = {|"alcotest" {with-test}|}

let%test "dep_to_string: with-doc no bound" =
  dep_to_string
    { name = "odoc"; scope = With_doc; bound = Simple Unconstrained }
  = {|"odoc" {with-doc}|}

let%test "dep_to_string: skip emits verbatim" =
  let s = {|"foo" {>= "1.0" & os = "linux"}|} in
  dep_to_string { name = "foo"; scope = Runtime; bound = Skip s } = s

(* --- equality unit tests --- *)

let%test "simple_bound_equal: reflexive for all constructors" =
  simple_bound_equal Unconstrained Unconstrained
  && simple_bound_equal (At_least "1.0") (At_least "1.0")
  && simple_bound_equal (Below "5.0") (Below "5.0")
  && simple_bound_equal (Bounded ("1.0", "5.0")) (Bounded ("1.0", "5.0"))

let%test "simple_bound_equal: different constructors" =
  not (simple_bound_equal (At_least "1.0") (Below "1.0"))

let%test "dep_bound_equal: Simple vs Ocaml_split" =
  not
    (dep_bound_equal (Simple (At_least "1.0"))
       (Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "1.0" }))

let%test "dep_equal: same dep" =
  let derp =
    { name = "foo"; scope = Runtime; bound = Simple (At_least "1.0") }
  in
  dep_equal derp derp

let%test "dep_equal: different names" =
  not
    (dep_equal
       { name = "foo"; scope = Runtime; bound = Simple Unconstrained }
       { name = "bar"; scope = Runtime; bound = Simple Unconstrained })

let%test "dep_equal: different scopes" =
  not
    (dep_equal
       { name = "foo"; scope = Runtime; bound = Simple Unconstrained }
       { name = "foo"; scope = With_test; bound = Simple Unconstrained })

let%test "dep_equal: different bounds" =
  not
    (dep_equal
       { name = "foo"; scope = Runtime; bound = Simple (At_least "1.0") }
       { name = "foo"; scope = Runtime; bound = Simple (At_least "2.0") })

(* --- round-trip tests --- *)

let roundtrip dep_str =
  match read_deps dep_str with
  | [ dep ] -> (
      let emitted = dep_to_string dep in
      match read_deps emitted with [ dep2 ] -> dep_equal dep dep2 | _ -> false)
  | _ -> false

let roundtrip_expect dep_str =
  match read_deps dep_str with
  | [ dep ] ->
      let emitted = dep_to_string dep in
      print_endline emitted
  | _ -> ()

let%test "roundtrip: bare dep" = roundtrip {|"cmdliner"|}
let%test "roundtrip: lower bound" = roundtrip {|"cmdliner" {>= "1.1.0"}|}
let%test "roundtrip: upper bound" = roundtrip {|"ocaml" {< "5.0.0"}|}

let%test "roundtrip: bounded range" =
  roundtrip {|"ocaml" {>= "4.14.0" & < "5.0.0"}|}

let%test "roundtrip: with-test no bound" = roundtrip {|"alcotest" {with-test}|}
let%test "roundtrip: with-doc no bound" = roundtrip {|"odoc" {with-doc}|}

let%test "roundtrip: with-test lower bound" =
  roundtrip {|"alcotest" {with-test & >= "1.6"}|}

let%test "roundtrip: with-test bounded range" =
  roundtrip {|"alcotest" {with-test & >= "1.0" & < "3.0"}|}

let%expect_test "upgrade: ocaml-split collapses to higher bound" =
  roundtrip_expect
    {|"pkg" {>= "1.0" & ocaml:version < "5.0.0" | >= "2.0" & ocaml:version >= "5.0.0"}|};
  [%expect {| "pkg" {>= "2.0"} |}]

let%expect_test "upgrade: ocaml-split parens form collapses to higher bound" =
  roundtrip_expect
    {|"pkg" {(>= "1.0" & ocaml:version < "5.0.0") | (>= "2.0" & ocaml:version >= "5.0.0")}|};
  [%expect {| "pkg" {>= "2.0"} |}]

let%expect_test "upgrade: scope + split repeated form collapses to higher bound"
    =
  roundtrip_expect
    {|"pkg" {(with-test & >= "1.0" & ocaml:version < "5.0") | (with-test & >= "2.0" & ocaml:version >= "5.0")}|};
  [%expect {| "pkg" {with-test & >= "2.0"} |}]

let%expect_test "upgrade: scope + split factored form collapses to higher bound"
    =
  roundtrip_expect
    {|"pkg" {with-test & (>= "1.0" & ocaml:version < "5.0" | >= "2.0" & ocaml:version >= "5.0")}|};
  [%expect {| "pkg" {with-test & >= "2.0"} |}]

let%test "roundtrip: ocaml package split" =
  roundtrip {|"ocaml" {>= "4.02" & < "4.12" | >= "5.2.0"}|}

let%test "roundtrip: skip preserved" =
  roundtrip {|"foo" {>= "1.0" & os = "linux"}|}

let%expect_test "roundtrip: ocaml package split" =
  roundtrip_expect {|"ocaml" {>= "4.02" & < "4.12" | >= "5.2.0"}|};
  [%expect {| "ocaml" {>= "4.02" & < "4.12" | >= "5.2.0"} |}]

let%expect_test "roundtrip: ocaml package split simpler" =
  roundtrip_expect {|"ocaml" {>= "4.02" & < "5.0.0" | >= "5.2.0"}|};
  [%expect {| "ocaml" {>= "4.02" & < "5.0.0" | >= "5.2.0"} |}]

let%expect_test
    "roundtrip: ocaml dep with bare ocaml:version guard (Unconstrained o4)" =
  roundtrip_expect
    {|"ocaml" {ocaml:version < "5.0.0" | >= "5.0.0" & ocaml:version >= "5.0.0"}|};
  [%expect {| "ocaml" {< "5.0.0" | >= "5.0.0"} |}]

let%expect_test "roundtrip: ocaml dep with only upper bound on o4 side (Below)"
    =
  roundtrip_expect {|"ocaml" {< "5.0.0" | >= "5.0.0"}|};
  [%expect {| "ocaml" {< "5.0.0" | >= "5.0.0"} |}]

let%expect_test
    "roundtrip: ocaml dep with different upper bound on o4 side (Below)" =
  roundtrip_expect {|"ocaml" {< "4.14.0" | >= "5.0.0"}|};
  [%expect {| "ocaml" {< "4.14.0" | >= "5.0.0"} |}]

(* --- merge_dep_bound tests --- *)

(* Simple + one OCaml major: stays Simple, absorbs the lower bound. *)

let%test "merge_dep_bound: Simple Unconstrained + o4 only → At_least" =
  dep_bound_equal
    (merge_dep_bound (Simple Unconstrained) (Some "1.0") None)
    (Simple (At_least "1.0"))

let%test "merge_dep_bound: Simple Unconstrained + o5 only → At_least" =
  dep_bound_equal
    (merge_dep_bound (Simple Unconstrained) None (Some "2.0"))
    (Simple (At_least "2.0"))

let%test "merge_dep_bound: Simple At_least replaced by o4 lower" =
  dep_bound_equal
    (merge_dep_bound (Simple (At_least "0.9")) (Some "1.0") None)
    (Simple (At_least "1.0"))

let%test "merge_dep_bound: Simple Below kept, lo added" =
  dep_bound_equal
    (merge_dep_bound (Simple (Below "5.0")) (Some "1.0") None)
    (Simple (Bounded ("1.0", "5.0")))

let%test "merge_dep_bound: Simple Bounded upper preserved, lo replaced" =
  dep_bound_equal
    (merge_dep_bound (Simple (Bounded ("0.5", "3.0"))) (Some "1.0") None)
    (Simple (Bounded ("1.0", "3.0")))

(* Simple + both OCaml majors with equal results: stays Simple. *)

let%test "merge_dep_bound: Simple + equal o4 and o5 → Simple" =
  dep_bound_equal
    (merge_dep_bound (Simple Unconstrained) (Some "1.0") (Some "1.0"))
    (Simple (At_least "1.0"))

(* Simple + both OCaml majors with different results: becomes Ocaml_split. *)

let%test "merge_dep_bound: Simple + different o4 and o5 → Ocaml_split" =
  dep_bound_equal
    (merge_dep_bound (Simple Unconstrained) (Some "1.0") (Some "2.0"))
    (Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" })

let%test "merge_dep_bound: Simple Below + different → Ocaml_split with upper" =
  dep_bound_equal
    (merge_dep_bound (Simple (Below "5.0")) (Some "1.0") (Some "2.0"))
    (Ocaml_split
       { ocaml4 = Bounded ("1.0", "5.0"); ocaml5 = Bounded ("2.0", "5.0") })

(* Ocaml_split + one OCaml major: collapses to Simple using the tested branch. *)

let%test "merge_dep_bound: Ocaml_split + o4 only → Simple from ocaml4 branch" =
  let split =
    Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" }
  in
  dep_bound_equal
    (merge_dep_bound split (Some "1.2") None)
    (Simple (At_least "1.2"))

let%test "merge_dep_bound: Ocaml_split + o5 only → Simple from ocaml5 branch" =
  let split =
    Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" }
  in
  dep_bound_equal
    (merge_dep_bound split None (Some "2.3"))
    (Simple (At_least "2.3"))

let%test
    "merge_dep_bound: Ocaml_split + o4 only preserves existing upper from \
     ocaml4 branch" =
  let split = Ocaml_split { ocaml4 = Below "4.0"; ocaml5 = At_least "2.0" } in
  dep_bound_equal
    (merge_dep_bound split (Some "1.0") None)
    (Simple (Bounded ("1.0", "4.0")))

(* Ocaml_split + both OCaml majors: each branch gets its result merged. *)

let%test "merge_dep_bound: Ocaml_split + both → Ocaml_split updated" =
  let split =
    Ocaml_split { ocaml4 = At_least "0.5"; ocaml5 = At_least "0.5" }
  in
  dep_bound_equal
    (merge_dep_bound split (Some "1.0") (Some "2.0"))
    (Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" })

let%test "merge_dep_bound: Ocaml_split + both equal → collapses to Simple" =
  let split =
    Ocaml_split { ocaml4 = At_least "0.5"; ocaml5 = At_least "0.5" }
  in
  dep_bound_equal
    (merge_dep_bound split (Some "1.0") (Some "1.0"))
    (Simple (At_least "1.0"))

(* Skip is always left unchanged regardless of versions. *)

let%test "merge_dep_bound: Skip unchanged with o4 only" =
  let s = {|"foo" {>= "1.0" & os = "linux"}|} in
  dep_bound_equal (merge_dep_bound (Skip s) (Some "2.0") None) (Skip s)

let%test "merge_dep_bound: Skip unchanged with both" =
  let s = {|"foo" {>= "1.0" & os = "linux"}|} in
  dep_bound_equal (merge_dep_bound (Skip s) (Some "2.0") (Some "3.0")) (Skip s)

(* --- dep_to_string expect test --- *)

let%expect_test "dep_to_string" =
  let check dep = print_endline (dep_to_string dep) in
  List.iter check
    [
      { name = "bare"; scope = Runtime; bound = Simple Unconstrained };
      { name = "lower"; scope = Runtime; bound = Simple (At_least "1.0") };
      { name = "upper"; scope = Runtime; bound = Simple (Below "5.0") };
      {
        name = "bounded";
        scope = Runtime;
        bound = Simple (Bounded ("1.0", "5.0"));
      };
      { name = "test"; scope = With_test; bound = Simple Unconstrained };
      { name = "doc"; scope = With_doc; bound = Simple Unconstrained };
      {
        name = "test-lower";
        scope = With_test;
        bound = Simple (At_least "1.6");
      };
      { name = "doc-lower"; scope = With_doc; bound = Simple (At_least "1.6") };
      {
        name = "split";
        scope = Runtime;
        bound = Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" };
      };
      {
        name = "split-unc4";
        scope = Runtime;
        bound = Ocaml_split { ocaml4 = Unconstrained; ocaml5 = At_least "2.0" };
      };
      {
        name = "test-split";
        scope = With_test;
        bound = Ocaml_split { ocaml4 = At_least "1.0"; ocaml5 = At_least "2.0" };
      };
    ];
  [%expect
    {|
    "bare"
    "lower" {>= "1.0"}
    "upper" {< "5.0"}
    "bounded" {>= "1.0" & < "5.0"}
    "test" {with-test}
    "doc" {with-doc}
    "test-lower" {with-test & >= "1.6"}
    "doc-lower" {with-doc & >= "1.6"}
    "split" {>= "2.0"}
    "split-unc4" {>= "2.0"}
    "test-split" {with-test & >= "2.0"}
    |}]
