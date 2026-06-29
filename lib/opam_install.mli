(** [deps ~switch ~dir] installs the dependencies of the opam project rooted
    at [dir] into [switch], without installing the project itself. --with-doc
    is not included. *)
val deps : switch:string -> dir:string -> (unit, string) result
