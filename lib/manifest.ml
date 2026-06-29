open Printf

(* Dep types and functions. *)

type simple_bound =
  | Unconstrained
  | At_least of string
  | Below of string
  | Bounded of string * string

type dep_bound =
  | Simple of simple_bound
  | Ocaml_split of { ocaml4 : simple_bound; ocaml5 : simple_bound }
  | Skip of string

type dep_scope = Runtime | With_test | With_doc
type dep = { name : string; scope : dep_scope; bound : dep_bound }

let simple_bound_to_string = function
  | Unconstrained -> ""
  | At_least v -> sprintf {|>= "%s"|} v
  | Below v -> sprintf {|< "%s"|} v
  | Bounded (lo, hi) -> sprintf {|>= "%s" & < "%s"|} lo hi

let simple_bound_equal s1 s2 =
  match (s1, s2) with
  | Unconstrained, Unconstrained -> true
  | At_least v1, At_least v2 -> String.equal v1 v2
  | Below v1, Below v2 -> String.equal v1 v2
  | Bounded (l1, h1), Bounded (l2, h2) ->
      String.equal l1 l2 && String.equal h1 h2
  | _, _ -> false

let merge_simple_bound s min_version =
  match s with
  | Unconstrained | At_least _ -> At_least min_version
  | Below max_version -> Bounded (min_version, max_version)
  | Bounded (_, hi) -> Bounded (min_version, hi)

let simplify_dep_bound db =
  match db with
  | Simple _ -> db
  | Ocaml_split { ocaml4; ocaml5 } ->
      if simple_bound_equal ocaml4 ocaml5 then Simple ocaml4 else db
  | Skip _ -> db

let lower_version_of = function
  | At_least v | Bounded (v, _) -> Some v
  | Unconstrained | Below _ -> None

let higher_simple_bound s1 s2 =
  match (lower_version_of s1, lower_version_of s2) with
  | None, _ -> s2
  | _, None -> s1
  | Some v1, Some v2 ->
      if Version.compare (Version.of_string v1) (Version.of_string v2) >= 0 then
        s1
      else s2

let dep_bound_to_string = function
  | Simple sb -> simple_bound_to_string sb
  | Ocaml_split { ocaml4; ocaml5 } ->
      simple_bound_to_string (higher_simple_bound ocaml4 ocaml5)
  | Skip s -> s

let dep_bound_equal db1 db2 =
  match (db1, db2) with
  | Simple s1, Simple s2 -> simple_bound_equal s1 s2
  | Ocaml_split o1, Ocaml_split o2 ->
      simple_bound_equal o1.ocaml4 o2.ocaml4
      && simple_bound_equal o1.ocaml5 o2.ocaml5
  | Skip s1, Skip s2 -> String.equal s1 s2
  | _, _ -> false

let merge_dep_bound dep o4version o5version =
  let newdep =
    match (dep, o4version, o5version) with
    (* One OCaml major version was excluded by the dep constraint or had no
       passing compiler, so only one result exists and no split needed. *)
    | Simple s, Some ov, None | Simple s, None, Some ov ->
        Simple (merge_simple_bound s ov)
    | Simple s, Some o4, Some o5 when String.(equal o4 o5) ->
        Simple (merge_simple_bound s o4)
    | Simple s, Some o4, Some o5 ->
        let ocaml4 = merge_simple_bound s o4 in
        let ocaml5 = merge_simple_bound s o5 in
        Ocaml_split { ocaml4; ocaml5 }
    (* Unreachable: probe_ocaml_versions returns Error when both are None,
       so run() exits before this function is ever called. *)
    | Simple _, None, None -> assert false
    (* Existing split collapses when only one OCaml major was tested: apply
       the min version to whichever branch was actually exercised. *)
    | Ocaml_split o, Some ov, None -> Simple (merge_simple_bound o.ocaml4 ov)
    | Ocaml_split o, None, Some ov -> Simple (merge_simple_bound o.ocaml5 ov)
    | Ocaml_split o, Some o4, Some o5 ->
        let ocaml4 = merge_simple_bound o.ocaml4 o4 in
        let ocaml5 = merge_simple_bound o.ocaml5 o5 in
        Ocaml_split { ocaml4; ocaml5 }
    | Ocaml_split _, None, None -> assert false
    | Skip _, _, _ -> dep
  in
  (* Collapse equal split branches to Simple. *)
  simplify_dep_bound newdep

let dep_scope_to_string scope bound =
  let bound_s = dep_bound_to_string bound in
  let contains_or = String.contains bound_s '|' in
  match (scope, bound) with
  | Runtime, _ -> bound_s
  | With_test, Simple Unconstrained -> "with-test"
  | With_doc, Simple Unconstrained -> "with-doc"
  | With_test, _ ->
      if contains_or then sprintf "with-test & (%s)" bound_s
      else "with-test & " ^ bound_s
  | With_doc, _ ->
      if contains_or then sprintf "with-doc & (%s)" bound_s
      else "with-doc & " ^ bound_s

let dep_scope_equal = ( = )

let dep_to_string dep =
  match (dep.name, dep.scope, dep.bound) with
  | "ocaml", _, Ocaml_split { ocaml4; ocaml5 } -> begin
      let extra_upper_bound =
        match ocaml4 with
        | At_least _ -> {| & < "5.0.0"|}
        | Unconstrained -> {|< "5.0.0"|}
        | Bounded _ | Below _ -> ""
      in
      sprintf {|"ocaml" {%s%s | %s}|}
        (simple_bound_to_string ocaml4)
        extra_upper_bound
        (simple_bound_to_string ocaml5)
    end
  | _, _, Skip s -> s
  | _, Runtime, Simple Unconstrained -> sprintf {|"%s"|} dep.name
  | _ ->
      sprintf {|"%s" {%s}|} dep.name (dep_scope_to_string dep.scope dep.bound)

let dep_equal d1 d2 =
  String.equal d1.name d2.name
  && dep_scope_equal d1.scope d2.scope
  && dep_bound_equal d1.bound d2.bound

(* Manifest type. *)

type t = {
  path : string;
  lines : string array;
  dep_range : int * int;
  patchable : bool;
  parsed_deps : dep list;
}

let deps t = t.parsed_deps
let dep_range t = t.dep_range
let patchable t = t.patchable

(* Writing out. *)

let dep_section_lines deps =
  [ "depends: [" ]
  @ List.map (fun dep -> "  " ^ dep_to_string dep) deps
  @ [ "]" ]

let write_out t deps =
  if not t.patchable then Error "Manifest file isn't patchable"
  else
    let temppath = t.path ^ ".tmp" in
    let pre_lines = Array.sub t.lines 0 (fst t.dep_range) |> Array.to_list in
    let post_lines =
      Array.sub t.lines (snd t.dep_range)
        (Array.length t.lines - snd t.dep_range)
      |> Array.to_list
    in
    let outlines = pre_lines @ dep_section_lines deps @ post_lines in

    let created = ref false in
    let cleanup () =
      if !created then try Sys.remove temppath with Sys_error _ -> ()
    in
    try
      Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_excl ]
        0o644 temppath (fun oc ->
          created := true;
          List.iter
            (fun line ->
              Out_channel.output_string oc line;
              Out_channel.output_char oc '\n')
            outlines);
      Unix.rename temppath t.path;
      Ok ()
    with
    | Sys_error msg ->
        cleanup ();
        Error (sprintf "could not write %s: %s" temppath msg)
    | Unix.Unix_error (err, fn, _) ->
        cleanup ();
        Error
          (sprintf "could not rename %s: %s: %s" temppath fn
             (Unix.error_message err))

(* Helper functions. *)

let apply_filter (s : simple_bound) (version_list : Version.t list) =
  match s with
  | Unconstrained -> version_list
  | At_least lo ->
      let min_v = Version.of_string lo in
      List.filter (fun v -> Version.compare min_v v <= 0) version_list
  | Below hi ->
      let max_v = Version.of_string hi in
      List.filter (fun v -> Version.compare max_v v > 0) version_list
  | Bounded (lo, hi) ->
      let min_v = Version.of_string lo in
      let max_v = Version.of_string hi in
      List.filter
        (fun pv ->
          Version.compare min_v pv <= 0 && Version.compare max_v pv > 0)
        version_list

let filter_dep_versions ocamlv (dep : dep) version_list =
  match dep.bound with
  | Simple s -> apply_filter s version_list
  | Ocaml_split split ->
      begin match ocamlv with
      | `Ocaml4 -> apply_filter split.ocaml4 version_list
      | `Ocaml5 -> apply_filter split.ocaml5 version_list
      end
  | Skip _ -> []

let split_report (deps : dep list) =
  List.filter_map
    (fun dep ->
      match dep.bound with
      | Ocaml_split { ocaml4; ocaml5 } -> Some (dep.name, ocaml4, ocaml5)
      | _ -> None)
    deps

let original_dep_lines t =
  Array.sub t.lines (fst t.dep_range) (snd t.dep_range - fst t.dep_range)
  |> Array.to_list

let split_deps (deps : dep list) =
  match
    List.fold_left
      (fun (ocamldep, acc) (item : dep) ->
        match item.name with
        | "ocaml" ->
            if Option.is_some ocamldep then
              failwith "ocaml listed as a dep more than once";
            (Some item, acc)
        | _ -> (ocamldep, item :: acc))
      (None, []) deps
  with
  | ocamldep, rest -> Ok (ocamldep, List.rev rest)
  | exception Failure msg -> Error msg
