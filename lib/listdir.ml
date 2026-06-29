let with_opendir path ~f =
  let d = Unix.opendir path in
  Fun.protect (fun () -> f d) ~finally:(fun () -> Unix.closedir d)

let listdir ?(skipsort = false) path =
  let open Unix in
  with_opendir path ~f:(fun d ->
      let rec loop acc =
        try
          let item = readdir d in
          match item with
          (* We are not interested in these items. *)
          | "." | ".." -> loop acc
          | _ -> loop (item :: acc)
        with End_of_file -> acc
      in
      match skipsort with
      | false -> loop [] |> List.sort String.compare
      | true -> loop [])

let pathlistdir ?(skipsort = false) path =
  listdir ~skipsort path |> List.map (fun f -> Filename.concat path f)
