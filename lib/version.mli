type t

val of_string : string -> t

val compare : t -> t -> int

val to_string_list : t -> string list

val to_string : t -> string

val major : t -> int

val is_stable : t -> bool

(** Sort by version, filter to stable releases, and deduplicate a
    list of raw version strings. [+] build variants are stripped before
    deduplication so [4.14.0] and [4.14.0+flambda] count as one version. *)
val sort_and_filter : t list -> t list