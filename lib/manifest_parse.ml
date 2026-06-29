open Manifest
open Result_syntax

(* Create a local AST type without all the position-tracking noise of the
   OpamParserTypes types. *)
type relop =
  [ `Eq  (** [=] *)
  | `Neq  (** [!=] *)
  | `Geq  (** [>=] *)
  | `Gt  (** [>] *)
  | `Leq  (** [<=] *)
  | `Lt  (** [<] *) ]

type logop = [ `And  (** [&] *) | `Or  (** [|] *) ]
type pfxop = [ `Not  (** [!] *) | `Defined  (** [?] *) ]
type env_update_op = OpamParserTypes.env_update_op

type value =
  | Bool of bool
  | Int of int
  | String of string
  | Relop of relop * value * value
  | Prefix_relop of relop * value
  | Logop of logop * value * value
  | Pfxop of pfxop * value
  | Ident of string
  | List of value list
  | Group of value list
  | Option of value * value list
  | Env_binding of value * env_update_op * value

let rec transform_ast (v : OpamParserTypes.FullPos.value) : value =
  match v.pelem with
  | Bool b -> Bool b
  | Int i -> Int i
  | String s -> String s
  | Relop (r, v1, v2) -> Relop (r.pelem, transform_ast v1, transform_ast v2)
  | Prefix_relop (r, v) -> Prefix_relop (r.pelem, transform_ast v)
  | Logop (l, v1, v2) -> Logop (l.pelem, transform_ast v1, transform_ast v2)
  | Pfxop (p, v) -> Pfxop (p.pelem, transform_ast v)
  | Ident i -> Ident i
  | List l -> List (List.map transform_ast l.pelem)
  | Group l -> Group (List.map transform_ast l.pelem)
  | Option (o, l) -> Option (transform_ast o, List.map transform_ast l.pelem)
  | Env_binding (v1, e, v2) ->
      Env_binding (transform_ast v1, e.pelem, transform_ast v2)

exception Bail

let extract_string (value : value) =
  match value with String s -> s | _ -> assert false

(* Merge parse trees of And and groups into a list. Keeps ors separate.
   Eg, if you have A & (B | C) & D, this will result in a list list
   [A; (B | C); D], where every item in the list can be Anded together. *)
let rec merge_tree = function
  | Logop (`And, v1, v2) -> merge_tree v1 @ merge_tree v2
  (* In practice, Groups always have a single item. *)
  | Group items -> List.concat_map (fun v -> merge_tree v) items
  | x -> [ x ]

(* Walk a value list and extract all the information we care about. *)
let atoms_to_scope_and_bound atoms =
  let scope = ref Runtime in
  let lo = ref None in
  let hi = ref None in
  List.iter
    (function
      | Ident "with-test" -> scope := With_test
      | Ident "with-doc" -> scope := With_doc
      | Ident _ -> raise Bail
      | Prefix_relop (`Geq, String v) -> lo := Some v
      | Prefix_relop (`Lt, String v) -> hi := Some v
      | Prefix_relop _ -> raise Bail
      | _ -> raise Bail)
    atoms;
  let bound =
    match (!lo, !hi) with
    | None, None -> Unconstrained
    | Some lo, None -> At_least lo
    | None, Some hi -> Below hi
    | Some lo, Some hi -> Bounded (lo, hi)
  in
  (!scope, bound)

(* Classify a branch of the form 'ocaml:version < "5.0"' or
   'ocaml:version > "5.0.0"'. *)
let classify_ocaml_branch atoms =
  let is_lt5 = function
    | Relop (`Lt, Ident "ocaml:version", String v)
      when String.length v > 0 && v.[0] = '5' ->
        true
    | _ -> false
  in
  let is_geq5 = function
    | Relop (`Geq, Ident "ocaml:version", String v)
      when String.length v > 0 && v.[0] = '5' ->
        true
    | _ -> false
  in
  if List.exists is_lt5 atoms then Some `Ocaml4
  else if List.exists is_geq5 atoms then Some `Ocaml5
  else None

(* When no ocaml:version guard is present, classify an ocaml-package branch by
   inspecting version numbers directly: a < "5.*" upper bound means ocaml4; a
   >= "5.*" lower bound means ocaml5. *)
let classify_ocaml_version_atoms atoms =
  if
    List.exists
      (function
        | Prefix_relop (`Lt, String v) when String.length v > 0 && v.[0] = '5'
          ->
            true
        | _ -> false)
      atoms
  then Some `Ocaml4
  else if
    List.exists
      (function
        | Prefix_relop (`Geq, String v) when String.length v > 0 && v.[0] = '5'
          ->
            true
        | _ -> false)
      atoms
  then Some `Ocaml5
  else None

let strip_ocaml_v =
  List.filter (function
    | Relop (_, Ident "ocaml:version", _) -> false
    | _ -> true)

(* Parse the AST of each opam dependency entry into a dep result. There's a
   limit to the complexity of the bounds format we'll support, up to split
   bounds for ocaml4 and ocaml5, and the bounds must be >= & < at the most.
   For anything else we bail, but that simply passes the bound through to the
   output unchanged. *)
let classify_dep (v : OpamParserTypes.FullPos.value) : (dep, string) result =
  (* Transform the OpamParserTypes value into one of our simplified values for
     easier parsing, but keep the original around in case of a Skip. *)
  let transformed_v = transform_ast v in
  match transformed_v with
  (* eg. "cmdliner" by itself *)
  | String name -> Ok { name; scope = Runtime; bound = Simple Unconstrained }
  (* eg. "cmdliner" {filter_expr_value} *)
  | Option (name_value, filter_expr_value) -> (
      let name = extract_string name_value in
      try
        let filter_expr =
          (* Opam dep filters always have exactly one element. *)
          match filter_expr_value with
          | [] -> raise Bail
          | expr :: _ -> expr
        in
        match filter_expr with
        (* eg. "cmdliner" {>= "0.9.4"} or {< "3.0.0"} *)
        | Prefix_relop (relop_value, version_value) -> begin
            let bound =
              match (relop_value, version_value) with
              | `Geq, String v -> At_least v
              | `Lt, String v -> Below v
              (* We don't support anything other than >= or <, but will pass
                 through unchanged anything else. *)
              | _ -> raise Bail
            in
            Ok { name; scope = Runtime; bound = Simple bound }
          end
        (* eg. "cmdliner" {with-test} *)
        | Ident "with-test" ->
            Ok { name; scope = With_test; bound = Simple Unconstrained }
        (* eg. "odoc" {with-doc} *)
        | Ident "with-doc" ->
            Ok { name; scope = With_doc; bound = Simple Unconstrained }
        (* Any top-level &. *)
        | Logop (`And, _, _) as and_expr -> (
            (* merge_tree returns a list of atoms that must be Anded together.
               Or atoms are kept as a separate tree. *)
            let atoms = merge_tree and_expr in
            let or_atoms, non_or_atoms =
              List.partition
                (function Logop (`Or, _, _) -> true | _ -> false)
                atoms
            in
            (* Eg. "pkg"
            {with-test &
              (>= "1.0" & ocaml:version < "5.0"
              | >= "2.0" & ocaml:version >= "5.0") *)
            match or_atoms with
            | [ Logop (_, branch_a, branch_b) ] ->
                let atoms_a = non_or_atoms @ merge_tree branch_a in
                let atoms_b = non_or_atoms @ merge_tree branch_b in
                let ocaml4_atoms, ocaml5_atoms =
                  match
                    ( classify_ocaml_branch atoms_a,
                      classify_ocaml_branch atoms_b )
                  with
                  | Some `Ocaml4, Some `Ocaml5 -> (atoms_a, atoms_b)
                  | Some `Ocaml5, Some `Ocaml4 -> (atoms_b, atoms_a)
                  (* We don't expect to see this case, because it'd require a
                     string like "ocaml" {with-test &
                      (>= "4.11.0" & < "5.0"
                      | >= "5.1.0" & < "5.4.0")
                     and specifying ocaml to be with-test doesn't make sense. *)
                  | None, None when name = "ocaml" ->
                      begin match
                        ( classify_ocaml_version_atoms atoms_a,
                          classify_ocaml_version_atoms atoms_b )
                      with
                      | Some `Ocaml4, Some `Ocaml5 -> (atoms_a, atoms_b)
                      | Some `Ocaml5, Some `Ocaml4 -> (atoms_b, atoms_a)
                      | _ -> (atoms_a, atoms_b)
                      end
                  (* This is a complex bounds expression that we will simply
                     pass through to the output unchanged. *)
                  | _ -> raise Bail
                in
                let scope4, bound4 =
                  atoms_to_scope_and_bound (strip_ocaml_v ocaml4_atoms)
                in
                let scope5, bound5 =
                  atoms_to_scope_and_bound (strip_ocaml_v ocaml5_atoms)
                in
                if scope4 <> scope5 then raise Bail;
                Ok
                  {
                    name;
                    scope = scope4;
                    bound = Ocaml_split { ocaml4 = bound4; ocaml5 = bound5 };
                  }
            (* No Or case, just a simple list of Anded conditions. *)
            | [] ->
                let scope, bound = atoms_to_scope_and_bound atoms in
                Ok { name; scope; bound = Simple bound }
            | _ -> raise Bail)
        (* Any top-level |. *)
        | Logop (`Or, branch_a, branch_b) ->
            let atoms_a = merge_tree branch_a in
            let atoms_b = merge_tree branch_b in
            let ocaml4_atoms, ocaml5_atoms =
              match
                (classify_ocaml_branch atoms_a, classify_ocaml_branch atoms_b)
              with
              | Some `Ocaml4, Some `Ocaml5 -> (atoms_a, atoms_b)
              | Some `Ocaml5, Some `Ocaml4 -> (atoms_b, atoms_a)
              (* We can see this case, because it matches eg.
                "ocaml" {>= "4.11.0" & < "5.0" | >= "5.1.0"}
                classify_ocaml_version_atoms checks which branch is which. *)
              | None, None when name = "ocaml" ->
                  begin match
                    ( classify_ocaml_version_atoms atoms_a,
                      classify_ocaml_version_atoms atoms_b )
                  with
                  | Some `Ocaml4, Some `Ocaml5 -> (atoms_a, atoms_b)
                  | Some `Ocaml5, Some `Ocaml4 -> (atoms_b, atoms_a)
                  | _ -> (atoms_a, atoms_b)
                  end
              | _ -> raise Bail
            in
            let scope4, bound4 =
              atoms_to_scope_and_bound (strip_ocaml_v ocaml4_atoms)
            in
            let scope5, bound5 =
              atoms_to_scope_and_bound (strip_ocaml_v ocaml5_atoms)
            in
            if scope4 <> scope5 then raise Bail;
            Ok
              {
                name;
                scope = scope4;
                bound = Ocaml_split { ocaml4 = bound4; ocaml5 = bound5 };
              }
        | _ -> raise Bail
      with Bail ->
        Ok { name; scope = Runtime; bound = Skip (OpamPrinter.FullPos.value v) }
      )
  | _ -> Error ("unrecognised depends entry: " ^ OpamPrinter.FullPos.value v)

let parse_value s =
  match OpamParser.FullPos.value_from_string s "<input>" with
  | exception exn ->
      let bt = Printexc.get_backtrace () in
      let msg = Printexc.to_string exn in
      Error (msg ^ if bt = "" then "" else "\n" ^ bt)
  | v -> Ok (transform_ast v)

let find_depends (items : OpamParserTypes.FullPos.opamfile_item list) =
  let open OpamParserTypes.FullPos in
  match
    List.find_opt
      (fun item ->
        match item.pelem with
        | Variable (k, _) -> k.pelem = "depends"
        | _ -> false)
      items
  with
  | None -> None
  | Some item -> begin
      let start_line = fst item.pos.start in
      let end_line = fst item.pos.stop in
      match item.pelem with
      | Variable (_, v) -> Some (v, (start_line, end_line))
      | _ -> assert false
    end

let check_patchable (dep_start, dep_end) items =
  let open OpamParserTypes.FullPos in
  List.for_all
    (fun item ->
      match item.pelem with
      | Variable (k, _) when k.pelem = "depends" -> true
      | _ ->
          let s = fst item.pos.start in
          let e = fst item.pos.stop in
          e < dep_start || s > dep_end)
    items

let collect_deps items =
  List.fold_left
    (fun acc item ->
      let* deps = acc in
      let* dep = classify_dep item in
      Ok (dep :: deps))
    (Ok []) items
  |> Result.map List.rev

let read path =
  In_channel.with_open_text path (fun ic ->
      let lines =
        In_channel.input_all ic |> String.split_on_char '\n' |> Array.of_list
      in
      In_channel.seek ic 0L;
      match OpamParser.FullPos.channel ic path with
      | exception exn ->
          let bt = Printexc.get_backtrace () in
          let msg = Printexc.to_string exn in
          Error
            (Printf.sprintf "failed to parse %s: %s%s" path msg
               (if bt = "" then "" else "\n" ^ bt))
      | file -> (
          match find_depends file.file_contents with
          | None ->
              Ok
                {
                  path;
                  lines;
                  dep_range = (0, 0);
                  patchable = false;
                  parsed_deps = [];
                }
          | Some (v, ((start_line, end_line) as range)) ->
              let* parsed_deps =
                match v.pelem with
                | List items -> collect_deps items.pelem
                | _ -> Ok []
              in
              Ok
                {
                  path;
                  lines;
                  dep_range = (start_line - 1, end_line);
                  patchable = check_patchable range file.file_contents;
                  parsed_deps;
                }))
