(** Session context: fixed for the duration of a run. *)
type ctx = { state : State.t; dir : string; package : string; verbose : bool }

(** Raised when dune files reference libraries absent from the .opam file.
    This indicates a project-level error, not a version incompatibility. *)
exception Missing_libraries of string list

(** Convert a build result to [Ok]/[Error], raising [Missing_libraries] for
    the corresponding [Opam_build] error variant. *)
val run_build_step : (unit, Opam_build.build_error) result -> (unit, string) result

val binary_search :
  Version.t array ->
  [ `Pass | `Fail | `Unknown ] array ->
  int ->
  int ->
  (Version.t -> bool) ->
  Version.t option

(** Test a single OCaml compiler version; returns [true] if it passes.
    [save_on_fail] controls whether a failing result is persisted to the state
    file; pass [false] for the initial current-compiler check so a transient
    failure does not poison the cache. *)
val test_compiler_version : ?save_on_fail:bool -> ctx -> Version.t -> bool

val test_compiler : ctx -> Version.t array -> (Version.t * Version.t) option

(** Like [test_compiler] but checks the highest version first; if that fails,
    the whole OCaml 4 range is skipped without further probing. *)
val test_ocaml4 : ctx -> Version.t array -> (Version.t * Version.t) option

(** [test_dep ctx ocamlv switch_version dep all_dep_versions]
    binary-searches [all_dep_versions] (filtered by the dep's bound) for the
    lowest version that passes build and test in the switch for
    [switch_version]. Creates the switch lazily on first probe, if necessary.
    Pins the package to each tested version, unpins once after the search
    completes (via Fun.protect). *)
val test_dep :
  ctx ->
  [ `Ocaml4 | `Ocaml5 ] ->
  Version.t ->
  Manifest.dep ->
  Version.t list ->
  (Version.t option, string) result
