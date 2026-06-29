type t = {
  results : (string, (string, [ `Pass | `Fail ]) Hashtbl.t) Hashtbl.t;
  combined : (string, string) Hashtbl.t;
}

let empty () = { results = Hashtbl.create 8; combined = Hashtbl.create 2 }

let make_key ~dep ~ocaml_version =
  match ocaml_version with None -> dep | Some v -> dep ^ "@" ^ v

let lookup t ~dep ~ocaml_version ~version =
  let key = make_key ~dep ~ocaml_version in
  let result =
    match Hashtbl.find_opt t.results key with
    | None -> `Unknown
    | Some versions -> (
        match Hashtbl.find_opt versions version with
        | None -> `Unknown
        | Some `Pass -> `Pass
        | Some `Fail -> `Fail)
  in
  let label =
    match result with
    | `Pass -> "pass"
    | `Fail -> "fail"
    | `Unknown -> "unknown"
  in
  Logs.debug (fun m -> m "state lookup %s %s -> %s" key version label);
  result

let record t ~dep ~ocaml_version ~version result =
  let key = make_key ~dep ~ocaml_version in
  let label = match result with `Pass -> "pass" | `Fail -> "fail" in
  Logs.debug (fun m -> m "state record %s %s = %s" key version label);
  let versions =
    match Hashtbl.find_opt t.results key with
    | Some v -> v
    | None ->
        let v = Hashtbl.create 4 in
        Hashtbl.add t.results key v;
        v
  in
  Hashtbl.replace versions version result

let combined_done t ocaml_key fingerprint =
  match Hashtbl.find_opt t.combined ocaml_key with
  | Some fp -> String.equal fp fingerprint
  | None -> false

let record_combined t ocaml_key fingerprint =
  Hashtbl.replace t.combined ocaml_key fingerprint

let filename dir = Filename.concat dir "opam-minver.json"

let to_json t : OpamJson.t =
  let sort_pairs pairs =
    List.sort (fun (a, _) (b, _) -> String.compare a b) pairs
  in
  let dep_pairs =
    Hashtbl.fold
      (fun dep versions acc ->
        let version_pairs =
          Hashtbl.fold
            (fun version result acc ->
              let r : OpamJson.t =
                match result with
                | `Pass -> `String "pass"
                | `Fail -> `String "fail"
              in
              (version, r) :: acc)
            versions []
        in
        (dep, (`O (sort_pairs version_pairs) : OpamJson.t)) :: acc)
      t.results []
  in
  let combined_pairs =
    Hashtbl.fold
      (fun key fp acc -> (key, (`String fp : OpamJson.t)) :: acc)
      t.combined []
  in
  `O
    [
      ("results", `O (sort_pairs dep_pairs));
      ("combined", `O (sort_pairs combined_pairs));
    ]

let of_json (json : OpamJson.t) =
  let ( let* ) = Option.bind in
  let assoc_obj = function `O kvs -> Some kvs | _ -> None in
  let* kvs = assoc_obj json in
  let* dep_kvs = Option.bind (List.assoc_opt "results" kvs) assoc_obj in
  let t = empty () in
  List.iter
    (fun (dep, versions_json) ->
      match assoc_obj versions_json with
      | None -> ()
      | Some version_kvs ->
          List.iter
            (fun (version, result_json) ->
              let outcome =
                match result_json with
                | `String "pass" -> Some `Pass
                | `String "fail" -> Some `Fail
                | _ -> None
              in
              Option.iter (record t ~dep ~ocaml_version:None ~version) outcome)
            version_kvs)
    dep_kvs;
  Option.iter
    (fun combined_json ->
      match assoc_obj combined_json with
      | None -> ()
      | Some combined_kvs ->
          List.iter
            (fun (key, fp_json) ->
              match fp_json with
              | `String fp -> record_combined t key fp
              | _ -> ())
            combined_kvs)
    (List.assoc_opt "combined" kvs);
  Some t

let read_file path = In_channel.with_open_text path In_channel.input_all

let load ~dir =
  let path = filename dir in
  match read_file path with
  | exception Sys_error _ ->
      Logs.debug (fun m -> m "state load: %s not found, starting fresh" path);
      empty ()
  | contents -> (
      match OpamJson.of_string contents with
      | Some json ->
          Logs.debug (fun m -> m "state load: %s loaded" path);
          Option.value ~default:(empty ()) (of_json json)
      | None ->
          Logs.debug (fun m ->
              m "state load: %s unparseable, starting fresh" path);
          empty ())

let save ~dir t =
  let path = filename dir in
  let tmppath = path ^ ".tmp" in
  try
    Out_channel.with_open_gen [ Open_wronly; Open_creat; Open_trunc ]
      0o644 tmppath (fun oc ->
        Out_channel.output_string oc
          (OpamJson.to_string ~minify:false (to_json t)));
    Unix.rename tmppath path
  with
  | Sys_error msg -> Logs.warn (fun m -> m "state save: %s" msg)
  | Unix.Unix_error (err, fn, _) ->
      Logs.warn (fun m -> m "state save: %s: %s" fn (Unix.error_message err))
