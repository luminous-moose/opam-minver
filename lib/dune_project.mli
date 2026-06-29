(** Returns [true] if the [dune-project] file in [dir] has an active
    [(generate_opam_files)] stanza (absent or explicitly [true]). Returns
    [false] if the file is absent, the stanza is absent, or it is explicitly
    set to [false]. Returns an [Error] if the file exists but cannot be
    parsed. *)
val generates_opam_files : dir:string -> (bool, string) result
