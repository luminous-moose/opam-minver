open Opam_minver

let () = Process.am_test := true

(* of_string *)

let%test "parse simple three-part" =
  Version.of_string "1.2.3" |> Version.to_string_list = [ "1"; "2"; "3" ]

let%test "parse four parts" =
  Version.of_string "4.14.0" |> Version.to_string_list = [ "4"; "14"; "0" ]

let%test "+ variant strips suffix" =
  Version.of_string "4.14.0+flambda"
  |> Version.to_string_list = [ "4"; "14"; "0" ]

let%test_unit "~ prerelease parses" =
  let (_ : Version.t) = Version.of_string "5.0.0~alpha1" in
  ()

(* compare *)

let%test "equal versions compare 0" =
  Version.compare (Version.of_string "1.0.0") (Version.of_string "1.0.0") = 0

let%test "numerical not lexicographic" =
  Version.compare (Version.of_string "4.9.0") (Version.of_string "4.10.0") < 0

let%test "more dots" =
  Version.compare (Version.of_string "4.10") (Version.of_string "4.10.1") < 0

let%test "+ variant equals base" =
  Version.compare
    (Version.of_string "4.14.0+flambda")
    (Version.of_string "4.14.0")
  = 0

let%test "+ variants equal each other" =
  Version.compare
    (Version.of_string "4.14.0+flambda")
    (Version.of_string "4.14.0+options")
  = 0

let%test "~ prerelease before release" =
  Version.compare (Version.of_string "5.0.0~alpha1") (Version.of_string "5.0.0")
  < 0

let%test "~ prerelease before release with extra dot" =
  Version.compare
    (Version.of_string "5.0.0~alpha1")
    (Version.of_string "5.0.0.0")
  < 0

let%test "~ rc before release" =
  Version.compare (Version.of_string "5.0.0~rc1") (Version.of_string "5.0.0")
  < 0

let%test "~ alpha before beta" =
  Version.compare
    (Version.of_string "5.0.0~alpha1")
    (Version.of_string "5.0.0~beta1")
  < 0

let%test "~ beta before rc" =
  Version.compare
    (Version.of_string "5.0.0~beta1")
    (Version.of_string "5.0.0~rc1")
  < 0

(* is_stable *)

let%test "stable release is_stable" =
  Version.is_stable (Version.of_string "4.14.0")

let%test "+ variant is_stable" =
  Version.is_stable (Version.of_string "4.14.0+flambda")

let%test "~ prerelease is not stable" =
  not (Version.is_stable (Version.of_string "5.0.0~alpha1"))

let%test "~ rc is not stable" =
  not (Version.is_stable (Version.of_string "5.0.0~rc1"))

(* sort_strings *)

let sort_strings s =
  let vs = List.map Version.of_string s in
  Version.sort_and_filter vs |> List.map Version.to_string

let%test "sort_strings basic" =
  sort_strings [ "4.14.0"; "4.9.0"; "4.10.0" ] = [ "4.9.0"; "4.10.0"; "4.14.0" ]

let%test "sort_strings filters prereleases" =
  sort_strings [ "4.14.0"; "5.0.0~alpha1"; "5.0.0" ] = [ "4.14.0"; "5.0.0" ]

let%test "sort_strings two + variants deduplicated" =
  sort_strings [ "4.14.0"; "4.14.0+flambda" ] = [ "4.14.0" ]

let%test "sort_strings three + variants deduplicated" =
  (* Three consecutive equal versions should all collapse to one *)
  sort_strings [ "4.14.0"; "4.14.0+flambda"; "4.14.0+options" ] = [ "4.14.0" ]

(* v-prefix (Jane Street) *)

let%test_unit "v-prefix parses" =
  let (_ : Version.t) = Version.of_string "v0.14.2" in
  ()

let%test "v-prefix to_string preserves original" =
  Version.to_string (Version.of_string "v0.14.2") = "v0.14.2"

let%test "v-prefix major is 0" = Version.major (Version.of_string "v0.14.2") = 0
let%test "v-prefix is stable" = Version.is_stable (Version.of_string "v0.14.2")

let%test "v-prefix patch comparison" =
  Version.compare (Version.of_string "v0.14.1") (Version.of_string "v0.14.2")
  < 0

let%test "v-prefix minor comparison" =
  Version.compare (Version.of_string "v0.13.0") (Version.of_string "v0.14.0")
  < 0

let%test "v-prefix sort" =
  sort_strings [ "v0.15.0"; "v0.13.0"; "v0.14.0" ]
  = [ "v0.13.0"; "v0.14.0"; "v0.15.0" ]

(* v-prefix with four parts *)

let%test_unit "v-prefix four-part parses" =
  let (_ : Version.t) = Version.of_string "v1.90.6" in
  ()

let%test "v-prefix four-part to_string" =
  Version.to_string (Version.of_string "v1.90.6") = "v1.90.6"

(* v-prefix with ~ prerelease *)

let%test_unit "v-prefix tilde prerelease parses" =
  let (_ : Version.t) = Version.of_string "v0.9.14~beta.2" in
  ()

let%test "v-prefix tilde prerelease is not stable" =
  not (Version.is_stable (Version.of_string "v0.9.14~beta.2"))

let%test "v-prefix tilde prerelease sorts before release" =
  Version.compare
    (Version.of_string "v0.9.14~beta.2")
    (Version.of_string "v0.9.14")
  < 0

(* v-prefix with -N opam patch revision *)

let%test_unit "v-prefix with revision parses" =
  let (_ : Version.t) = Version.of_string "v0.16.0-1" in
  ()

let%test "v-prefix revision is stable" =
  Version.is_stable (Version.of_string "v0.16.0-1")

let%test "v-prefix revision greater than base" =
  Version.compare (Version.of_string "v0.16.0") (Version.of_string "v0.16.0-1")
  < 0

(* -N opam patch revision (no v-prefix) *)

let%test_unit "-N revision parses" =
  let (_ : Version.t) = Version.of_string "1.0.0-1" in
  ()

let%test "-N revision to_string preserves original" =
  Version.to_string (Version.of_string "1.0.0-1") = "1.0.0-1"

let%test "-N revision is stable" =
  Version.is_stable (Version.of_string "1.0.0-1")

let%test "-N revision greater than base" =
  Version.compare (Version.of_string "1.0.0") (Version.of_string "1.0.0-1") < 0

let%test "-N revisions order correctly" =
  Version.compare (Version.of_string "1.0.0-1") (Version.of_string "1.0.0-2")
  < 0

let%test "-N revision sort" =
  sort_strings [ "1.0.0-2"; "1.0.0"; "1.0.0-1" ]
  = [ "1.0.0"; "1.0.0-1"; "1.0.0-2" ]

(* N.N-N two-part with revision (e.g. cudf 0.9-1) *)

let%test_unit "two-part with revision parses" =
  let (_ : Version.t) = Version.of_string "0.9-1" in
  ()

let%test "two-part with revision greater than base" =
  Version.compare (Version.of_string "0.9") (Version.of_string "0.9-1") < 0

(* N-N format (e.g. conf-libev 4-11) *)

let%test_unit "N-N format parses" =
  let (_ : Version.t) = Version.of_string "4-11" in
  ()

let%test "N-N format is stable" = Version.is_stable (Version.of_string "4-11")

let%test "N-N format orders correctly" =
  Version.compare (Version.of_string "4-11") (Version.of_string "4-12") < 0

let%test "N-N sort" =
  sort_strings [ "4-13"; "4-11"; "4-12" ] = [ "4-11"; "4-12"; "4-13" ]

(* N-X.Y.Z format (e.g. fswatch 11-0.1.0) *)

let%test_unit "N-X.Y.Z format parses" =
  let (_ : Version.t) = Version.of_string "11-0.1.0" in
  ()

let%test "N-X.Y.Z is stable" = Version.is_stable (Version.of_string "11-0.1.0")

let%test "N-X.Y.Z sort" =
  sort_strings [ "11-0.1.3"; "11-0.1.0"; "11-0.1.1" ]
  = [ "11-0.1.0"; "11-0.1.1"; "11-0.1.3" ]

(* Date-with-dashes (e.g. lem 2020-06-03) *)

let%test_unit "date-with-dashes parses" =
  let (_ : Version.t) = Version.of_string "2020-06-03" in
  ()

let%test "date-with-dashes to_string preserves original" =
  Version.to_string (Version.of_string "2020-06-03") = "2020-06-03"

let%test "date-with-dashes is stable" =
  Version.is_stable (Version.of_string "2020-06-03")

let%test "date-with-dashes sort" =
  sort_strings [ "2022-12-10"; "2020-06-03"; "2025-03-13" ]
  = [ "2020-06-03"; "2022-12-10"; "2025-03-13" ]

(* Letter suffix without ~ (e.g. afl 2.52b) *)

let%test_unit "letter-suffix parses" =
  let (_ : Version.t) = Version.of_string "2.52b" in
  ()

let%test "letter-suffix to_string preserves original" =
  Version.to_string (Version.of_string "2.52b") = "2.52b"

let%test "letter-suffix is stable" =
  Version.is_stable (Version.of_string "2.52b")

let%test "letter-suffix sort" =
  sort_strings [ "2.57b"; "2.52b" ] = [ "2.52b"; "2.57b" ]

(* Letter infix (e.g. gd 1.0a5) *)

let%test_unit "letter-infix parses" =
  let (_ : Version.t) = Version.of_string "1.0a5" in
  ()

let%test "letter-infix is stable" =
  Version.is_stable (Version.of_string "1.0a5")

let%test "letter-infix sorts before next minor" =
  Version.compare (Version.of_string "1.0a5") (Version.of_string "1.1") < 0

(* pl patch level (e.g. cryptoverif 2.03pl1) *)

let%test_unit "pl-suffix parses" =
  let (_ : Version.t) = Version.of_string "2.03pl1" in
  ()

let%test "pl-suffix is stable" = Version.is_stable (Version.of_string "2.03pl1")

let%test "pl-suffix greater than base" =
  Version.compare (Version.of_string "2.03") (Version.of_string "2.03pl1") < 0

let%test "pl-suffix less than next version" =
  Version.compare (Version.of_string "2.03pl1") (Version.of_string "2.04") < 0

(* -rc suffix without ~ (e.g. binsec_codex 1.0-rc4) *)

let%test_unit "-rc suffix parses" =
  let (_ : Version.t) = Version.of_string "1.0-rc4" in
  ()

let%test "-rc is stable (only ~ marks pre-releases in opam)" =
  Version.is_stable (Version.of_string "1.0-rc4")

(* -beta.N suffix without ~ (e.g. herdtools7 7.42-beta.3) *)

let%test_unit "-beta.N suffix parses" =
  let (_ : Version.t) = Version.of_string "7.42-beta.3" in
  ()

let%test "-beta.N is stable (only ~ marks pre-releases in opam)" =
  Version.is_stable (Version.of_string "7.42-beta.3")

let%test "-beta.N sort" =
  sort_strings [ "7.43"; "7.42"; "7.42-beta.3" ]
  = [ "7.42"; "7.42-beta.3"; "7.43" ]

(* Single-letter opam patch revision (e.g. libbinaryen 117.0.0-b) *)

let%test_unit "single-letter revision parses" =
  let (_ : Version.t) = Version.of_string "117.0.0-b" in
  ()

let%test "single-letter revision is stable" =
  Version.is_stable (Version.of_string "117.0.0-b")

let%test "single-letter revision greater than base" =
  Version.compare (Version.of_string "117.0.0") (Version.of_string "117.0.0-b")
  < 0

(* Apron mixed scheme: plain date-based then v-prefixed *)

let%test "apron date-based versions sort correctly" =
  sort_strings [ "20150930"; "20150820"; "20160125"; "20151015"; "20160108" ]
  = [ "20150820"; "20150930"; "20151015"; "20160108"; "20160125" ]

let%test "apron date-based sorts before v-prefixed" =
  Version.compare (Version.of_string "20160125") (Version.of_string "v0.9.12")
  < 0

let%test "apron full history sorts correctly" =
  sort_strings
    [
      "20150820";
      "20150930";
      "20151015";
      "20160108";
      "20160125";
      "v0.9.12";
      "v0.9.13";
      "v0.9.14~beta.2";
      "v0.9.14";
      "v0.9.15";
    ]
  = [
      "20150820";
      "20150930";
      "20151015";
      "20160108";
      "20160125";
      "v0.9.12";
      "v0.9.13";
      "v0.9.14";
      "v0.9.15";
    ]

(* to_string *)

let%test "to_string preserves original string" =
  Version.to_string (Version.of_string "4.14.0") = "4.14.0"

let%test "to_string preserves + variant string" =
  Version.to_string (Version.of_string "4.14.0+flambda") = "4.14.0+flambda"

let%test "to_string preserves ~ prerelease string" =
  Version.to_string (Version.of_string "5.0.0~alpha1") = "5.0.0~alpha1"

(* major *)

let%test "major of 4.x" = Version.major (Version.of_string "4.14.0") = 4
let%test "major of 5.x" = Version.major (Version.of_string "5.2.0") = 5
let%test "major of 1.x" = Version.major (Version.of_string "1.0.0") = 1

(* expect test *)

let%expect_test "sort_strings" =
  let vs =
    [ "4.14.0"; "4.9.0"; "4.10.0"; "5.0.0~alpha1"; "5.0.0"; "4.14.0+flambda" ]
  in
  List.iter print_endline (sort_strings vs);
  [%expect {|
    4.9.0
    4.10.0
    4.14.0
    5.0.0
    |}]
