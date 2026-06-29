type build_error =
  | Missing_libraries of string list
  | Build_failure of string

val extract_missing_library : string -> string option
val classify_error : string -> build_error

(** [build ~switch ~dir ~package] runs [dune build -p package] for the
    project at [dir] using the environment of [switch]. Passing [-p] scopes
    the build to stanzas that belong to [package], which is necessary for
    projects whose dune files reference other packages not present in [dir]
    (e.g. with-dev-setup bench targets, or monorepo siblings). *)
val build : switch:string -> dir:string -> package:string -> (unit, build_error) result

val test : switch:string -> dir:string -> package:string -> (unit, build_error) result
