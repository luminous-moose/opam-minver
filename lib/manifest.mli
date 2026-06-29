
(** Note: opam-minver only discovers lower bounds. [Below] and [Bounded]
    are recognised and respected when already present in the opam file, but
    the tool will never add or tighten an upper bound itself. *)
type simple_bound =
  | Unconstrained                    (** no version constraint *)
  | At_least of string               (** [{>= "X"}] *)
  | Below of string                  (** [{< "X"}] - strict upper bound *)
  | Bounded of string * string       (** [{>= "X" & < "Y"}] *)

type dep_bound =
  | Simple of simple_bound
  (** Internal representation for a dependency whose minimum version differs
      between OCaml 4 and OCaml 5.  When serialised, the higher of the two
      bounds is emitted as a plain [Simple] constraint (opam cannot express
      per-major-version bounds via package-variable filters).  The split is
      reported to the user so they can act on it manually if desired. *)
  | Ocaml_split of {
      ocaml4 : simple_bound;
      ocaml5 : simple_bound;
    }
  (** Constraint uses filters we don't recognise (e.g. [os = "linux"]).
      The string holds the original serialised dependency entry.
      opam-minver will leave the entry unchanged. *)
  | Skip of string

type dep_scope =
  | Runtime    (** no scope filter, always required *)
  | With_test  (** [{with-test ...}] *)
  | With_doc   (** [{with-doc ...}] *)

type dep = {
  name  : string;
  scope : dep_scope;
  bound : dep_bound;
}

type t = {
  path        : string;
  lines       : string array;
  dep_range   : int * int;
  patchable   : bool;
  parsed_deps : dep list;
}

val deps : t -> dep list

(** [(start, stop)] line range of the [depends:] block as 0-based indices into
    [lines]: [start] is the index of the [depends: \[] line; [stop] is the
    exclusive end (index of the line after [\]]). Suitable for direct use with
    [Array.sub lines 0 start] and [Array.sub lines stop (n - stop)]. *)
val dep_range : t -> int * int

val patchable : t -> bool

val dep_section_lines : dep list -> string list

(** The original lines of the [depends:] block as they appear in the source
    file.  Use this to compare against [dep_section_lines new_deps] to detect
    whether the serialised output would actually change the file. *)
val original_dep_lines : t -> string list

(** Write [deps] back into the [depends:] block of the file at [t.path],
    replacing the original block.  Writes to a temp file first and renames
    atomically.  Returns [Error] if [t] is not patchable or any I/O fails. *)
val write_out : t -> dep list -> (unit, string) result

(** Pick the more restrictive of two simple bounds by comparing their lower
    versions.  A bound with a lower-version wins over [Unconstrained] or
    [Below]. *)
val higher_simple_bound : simple_bound -> simple_bound -> simple_bound

(** Return [(name, ocaml4_bound, ocaml5_bound)] for every dep whose bound is
    [Ocaml_split].  Used to warn the user that the higher minimum was chosen. *)
val split_report : dep list -> (string * simple_bound * simple_bound) list

val apply_filter : simple_bound -> Version.t list -> Version.t list

val filter_dep_versions :
  [ `Ocaml4 | `Ocaml5 ] ->
  dep ->
  Version.t list ->
  Version.t list

val split_deps : dep list -> (dep option * dep list, string) result

(** {2 Serialisation} *)

val simple_bound_to_string : simple_bound -> string

val simple_bound_equal : simple_bound -> simple_bound -> bool

val simplify_dep_bound : dep_bound -> dep_bound

val dep_bound_to_string : dep_bound -> string

val dep_bound_equal : dep_bound -> dep_bound -> bool

(** Merge discovered minimum version(s) into an existing dep bound.
    [o4version] and [o5version] are the minimum-passing versions for OCaml 4
    and OCaml 5 respectively; [None] means that major version was not tested
    or had no passing compiler.  At least one must be [Some].
    [Skip] deps are returned unchanged.
    The result is passed through [simplify_dep_bound], so equal split branches
    collapse to [Simple]. *)
val merge_dep_bound :
  dep_bound -> string option -> string option -> dep_bound

val dep_scope_to_string : dep_scope -> dep_bound -> string

val dep_to_string : dep -> string

val dep_equal : dep -> dep -> bool
