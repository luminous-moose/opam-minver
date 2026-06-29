val prefix : string

val create : name:string -> compiler:string -> (unit, string) result

val remove : name:string -> (unit, string) result

val list_ours : unit -> (string list, string) result

val find_or_create : name:string -> compiler:string -> (unit, string) result

val current_ocaml_version : unit -> (Version.t, string) result
