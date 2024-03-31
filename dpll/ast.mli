
type bin_op =
  | Or
  | And

type literal = int

(** AST: Операции И, Или, Отрицание и Литералы *)
(** Отрицание к переменной интерпретируется как литерал,
    Отрицание к выражению интерпертируется как Not exp, где exp логическое выражение  *)
type log_e = 
  | Operation of bin_op * log_e * log_e
  | Not of log_e
  | Literal of literal