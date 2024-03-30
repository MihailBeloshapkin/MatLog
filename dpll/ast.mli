
type bin_op =
  | Or
  | And

type literal = int

(** AST: Or, And operations and literals *)
type log_e = 
  | Operation of bin_op * log_e * log_e
  | Not of log_e
  | Literal of literal