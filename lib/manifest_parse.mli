type relop = [ `Eq | `Neq | `Geq | `Gt | `Leq | `Lt ]
type logop = [ `And | `Or ]
type pfxop = [ `Not | `Defined ]
type env_update_op = OpamParserTypes.env_update_op

type value =
  | Bool of bool
  | Int of int
  | String of string
  | Relop of relop * value * value
  | Prefix_relop of relop * value
  | Logop of logop * value * value
  | Pfxop of pfxop * value
  | Ident of string
  | List of value list
  | Group of value list
  | Option of value * value list
  | Env_binding of value * env_update_op * value

(** Parse a single opam value expression, e.g. ["cmdliner" {>= "1.0"}],
    and return the simplified AST. Useful for inspecting parse structure. *)
val parse_value : string -> (value, string) result

val read : string -> (Manifest.t, string) result
