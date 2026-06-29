open Opam_minver.Manifest_parse
module Sexp = Base.Sexp

let () = Opam_minver.Process.am_test := true

let sexp_of_relop = function
  | `Eq -> Sexp.Atom "Eq"
  | `Neq -> Sexp.Atom "Neq"
  | `Geq -> Sexp.Atom "Geq"
  | `Gt -> Sexp.Atom "Gt"
  | `Leq -> Sexp.Atom "Leq"
  | `Lt -> Sexp.Atom "Lt"

let sexp_of_logop = function `And -> Sexp.Atom "And" | `Or -> Sexp.Atom "Or"

let sexp_of_pfxop = function
  | `Not -> Sexp.Atom "Not"
  | `Defined -> Sexp.Atom "Defined"

let sexp_of_env_update_op (op : env_update_op) =
  match op with
  | Eq -> Sexp.Atom "Eq"
  | PlusEq -> Sexp.Atom "PlusEq"
  | EqPlus -> Sexp.Atom "EqPlus"
  | ColonEq -> Sexp.Atom "ColonEq"
  | EqColon -> Sexp.Atom "EqColon"
  | EqPlusEq -> Sexp.Atom "EqPlusEq"

let rec sexp_of_value = function
  | Bool b -> Sexp.List [ Sexp.Atom "Bool"; Sexp.Atom (string_of_bool b) ]
  | Int i -> Sexp.List [ Sexp.Atom "Int"; Sexp.Atom (string_of_int i) ]
  | String s -> Sexp.List [ Sexp.Atom "String"; Sexp.Atom s ]
  | Relop (op, v1, v2) ->
      Sexp.List
        [
          Sexp.Atom "Relop";
          sexp_of_relop op;
          sexp_of_value v1;
          sexp_of_value v2;
        ]
  | Prefix_relop (op, v) ->
      Sexp.List [ Sexp.Atom "Prefix_relop"; sexp_of_relop op; sexp_of_value v ]
  | Logop (op, v1, v2) ->
      Sexp.List
        [
          Sexp.Atom "Logop";
          sexp_of_logop op;
          sexp_of_value v1;
          sexp_of_value v2;
        ]
  | Pfxop (op, v) ->
      Sexp.List [ Sexp.Atom "Pfxop"; sexp_of_pfxop op; sexp_of_value v ]
  | Ident s -> Sexp.List [ Sexp.Atom "Ident"; Sexp.Atom s ]
  | List vs -> Sexp.List (Sexp.Atom "List" :: List.map sexp_of_value vs)
  | Group vs -> Sexp.List (Sexp.Atom "Group" :: List.map sexp_of_value vs)
  | Option (v, vs) ->
      Sexp.List
        [
          Sexp.Atom "Option";
          sexp_of_value v;
          Sexp.List (List.map sexp_of_value vs);
        ]
  | Env_binding (v1, op, v2) ->
      Sexp.List
        [
          Sexp.Atom "Env_binding";
          sexp_of_value v1;
          sexp_of_env_update_op op;
          sexp_of_value v2;
        ]

let show s =
  match parse_value s with
  | Ok v -> print_endline (Sexp.to_string_hum (sexp_of_value v))
  | Error e -> print_endline ("Error: " ^ e)

let%expect_test "bare dep" =
  show {|"cmdliner"|};
  [%expect {| (String cmdliner) |}]

let%expect_test "lower bound" =
  show {|"cmdliner" {>= "1.1.0"}|};
  [%expect {| (Option (String cmdliner) ((Prefix_relop Geq (String 1.1.0)))) |}]

let%expect_test "upper bound" =
  show {|"ocaml" {< "5.0.0"}|};
  [%expect {| (Option (String ocaml) ((Prefix_relop Lt (String 5.0.0)))) |}]

let%expect_test "bounded range" =
  show {|"ocaml" {>= "4.14.0" & < "5.0.0"}|};
  [%expect
    {|
    (Option (String ocaml)
     ((Logop And (Prefix_relop Geq (String 4.14.0))
       (Prefix_relop Lt (String 5.0.0)))))
    |}]

let%expect_test "with-test scope" =
  show {|"alcotest" {with-test}|};
  [%expect {| (Option (String alcotest) ((Ident with-test))) |}]

let%expect_test "with-doc scope" =
  show {|"odoc" {with-doc}|};
  [%expect {| (Option (String odoc) ((Ident with-doc))) |}]

let%expect_test "with-test and lower bound" =
  show {|"alcotest" {with-test & >= "1.6"}|};
  [%expect
    {|
    (Option (String alcotest)
     ((Logop And (Ident with-test) (Prefix_relop Geq (String 1.6)))))
    |}]

let%expect_test "with-test and bounded range" =
  show {|"alcotest" {with-test & >= "1.0" & < "3.0"}|};
  [%expect
    {|
    (Option (String alcotest)
     ((Logop And (Logop And (Ident with-test) (Prefix_relop Geq (String 1.0)))
       (Prefix_relop Lt (String 3.0)))))
    |}]

let%expect_test "ocaml-split or form" =
  show
    {|"pkg" {>= "1.0" & ocaml:version < "5.0" | >= "2.0" & ocaml:version >= "5.0"}|};
  [%expect
    {|
    (Option (String pkg)
     ((Logop Or
       (Logop And (Prefix_relop Geq (String 1.0))
        (Relop Lt (Ident ocaml:version) (String 5.0)))
       (Logop And (Prefix_relop Geq (String 2.0))
        (Relop Geq (Ident ocaml:version) (String 5.0))))))
    |}]

let%expect_test "ocaml-split with parens" =
  show
    {|"pkg" {(>= "1.0" & ocaml:version < "5.0") | (>= "2.0" & ocaml:version >= "5.0")}|};
  [%expect
    {|
    (Option (String pkg)
     ((Logop Or
       (Group
        (Logop And (Prefix_relop Geq (String 1.0))
         (Relop Lt (Ident ocaml:version) (String 5.0))))
       (Group
        (Logop And (Prefix_relop Geq (String 2.0))
         (Relop Geq (Ident ocaml:version) (String 5.0)))))))
    |}]

let%expect_test "ocaml package split without ocaml:version" =
  show {|"ocaml" {>= "4.02" & < "4.12" | >= "5.2.0"}|};
  [%expect
    {|
    (Option (String ocaml)
     ((Logop Or
       (Logop And (Prefix_relop Geq (String 4.02))
        (Prefix_relop Lt (String 4.12)))
       (Prefix_relop Geq (String 5.2.0)))))
    |}]

let%expect_test "scope and ocaml-split" =
  show
    {|"pkg" {(with-test & >= "1.0" & ocaml:version < "5.0") | (with-test & >= "2.0" & ocaml:version >= "5.0")}|};
  [%expect
    {|
    (Option (String pkg)
     ((Logop Or
       (Group
        (Logop And (Logop And (Ident with-test) (Prefix_relop Geq (String 1.0)))
         (Relop Lt (Ident ocaml:version) (String 5.0))))
       (Group
        (Logop And (Logop And (Ident with-test) (Prefix_relop Geq (String 2.0)))
         (Relop Geq (Ident ocaml:version) (String 5.0)))))))
    |}]

let%expect_test "factored scope and ocaml-split" =
  show
    {|"pkg" {with-test & (>= "1.0" & ocaml:version < "5.0" | >= "2.0" & ocaml:version >= "5.0")}|};
  [%expect
    {|
    (Option (String pkg)
     ((Logop And (Ident with-test)
       (Group
        (Logop Or
         (Logop And (Prefix_relop Geq (String 1.0))
          (Relop Lt (Ident ocaml:version) (String 5.0)))
         (Logop And (Prefix_relop Geq (String 2.0))
          (Relop Geq (Ident ocaml:version) (String 5.0))))))))
    |}]

let%expect_test "skip unknown filter" =
  show {|"foo" {>= "1.0" & os = "linux"}|};
  [%expect
    {|
    (Option (String foo)
     ((Logop And (Prefix_relop Geq (String 1.0))
       (Relop Eq (Ident os) (String linux)))))
    |}]
