(** Injectable environment for opam side-effects; substitute a stub in tests. *)
type probe_env = {
  ocaml_versions : unit -> (Version.t list, string) result;
  current_ocaml : unit -> (Version.t, string) result;
  dep_versions : string -> (Opam_versions.version_type, string) result;
  remove_all_pins : string -> (unit, string) result;
  run_combined_validation :
    package:string ->
    dir:string ->
    verbose:bool ->
    [ `Ocaml4 | `Ocaml5 ] ->
    Version.t ->
    (Manifest.dep * Version.t) list ->
    (unit, string) result;
}

(** Minimum passing version and the version used to test deps, for each
    OCaml major series. *)
type ocaml_probe_result = {
  ocaml4_min_and_test : (Version.t * Version.t) option;
  ocaml5_min_and_test : (Version.t * Version.t) option;
}

val filter_ocaml_versions :
  Manifest.dep option ->
  Version.t list ->
  Version.t ->
  (Version.t list * Version.t list, string) result

val probe_ocaml_versions :
  probe_env ->
  Probe.ctx ->
  Manifest.dep option ->
  (ocaml_probe_result, string) result

val probe_deps :
  probe_env ->
  Probe.ctx ->
  [ `Ocaml4 | `Ocaml5 ] ->
  Version.t ->
  Manifest.dep list ->
  ((Manifest.dep * Version.t) list, string) result

val merge_results :
  Manifest.dep list ->
  (Manifest.dep * Version.t) list option ->
  (Manifest.dep * Version.t) list option ->
  Manifest.dep list

val compute_bounds :
  probe_env ->
  State.t ->
  string ->
  verbose:bool ->
  package:string ->
  Manifest.dep list ->
  (Manifest.dep list, string) result

(** Top-level runner with an injectable [probe_env].  Computes bounds and
    either writes them to the opam file ([write_out:true]) or prints what would
    be written ([write_out:false]). *)
val run_with :
  probe_env ->
  State.t ->
  string ->
  ?preamble:string ->
  Manifest.t ->
  verbose:bool ->
  write_out:bool ->
  (unit, string) result

val run :
  State.t ->
  string ->
  Manifest.t ->
  preamble:string option ->
  verbose:bool ->
  write_out:bool ->
  (unit, string) result
