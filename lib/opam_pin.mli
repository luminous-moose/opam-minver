val add : switch:string -> package:string -> version:string -> (unit, string) result

val remove : switch:string -> package:string -> (unit, string) result

val remove_all : switch:string -> (unit, string) result
