type version_type = Contains_base | Versions of Version.t list

val available : package:string -> (version_type, string) result

val available_compilers : unit -> (Version.t list, string) result