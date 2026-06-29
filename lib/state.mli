(** Persistent search state, stored in [.opam-minver.json] in the project dir. *)

type t

val empty : unit -> t

(** Read [dir/.opam-minver.json]. Returns [empty ()] if the file is absent or
    unparseable. *)
val load : dir:string -> t

val save : dir:string -> t -> unit

val lookup :
  t ->
  dep:string ->
  ocaml_version:string option ->
  version:string ->
  [ `Pass | `Fail | `Unknown ]

val record :
  t ->
  dep:string ->
  ocaml_version:string option ->
  version:string ->
  [ `Pass | `Fail ] ->
  unit

(** [combined_done t ocaml_key fingerprint] returns [true] if combined
    validation was already recorded for [ocaml_key] ("ocaml4" or "ocaml5")
    with the given [fingerprint]. *)
val combined_done : t -> string -> string -> bool

(** Record that combined validation completed for [ocaml_key] with the dep
    versions summarised by [fingerprint]. *)
val record_combined : t -> string -> string -> unit
