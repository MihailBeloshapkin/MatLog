open Base
open Stdio
open Ast

type token =
  | Var_t of int
  | Or_t
  | Neg_t
  | And_t
  | Lparen_t
  | Rparen_t

let get_tokens str =
  let rec next_token tokens = function
    | [] -> tokens
    | '|' :: tl -> next_token (Or_t :: tokens) tl
    | '-' :: tl -> next_token (Neg_t :: tokens) tl
    | '&' :: tl -> next_token (And_t :: tokens) tl
    | '(' :: tl -> next_token (Lparen_t :: tokens) tl
    | ')' :: tl -> next_token (Rparen_t :: tokens) tl
    | c :: tl when Char.is_whitespace c -> next_token tokens tl
    | c :: tl when Char.is_digit c ->
      let num_list, tl = List.split_while ~f:(fun c -> Char.is_digit c) tl in
      let number = c :: num_list
                   |> String.of_char_list 
                   |> Int.of_string in
      next_token (Var_t (number) :: tokens) tl
    | _ -> raise (Failure "unexpected character")
  in
  let reversed = next_token [] (String.to_list str) in
  List.rev reversed

let token_to_str = function
  | Var_t value -> Printf.sprintf "Token[type: number, val: %d]" value
  | Or_t -> "Token[type: plus, val: +]"
  | Neg_t -> "Token[type: minus, val: -]"
  | And_t -> "Token[type: mul, val: *]"
  | Lparen_t -> "Token[type: lparen, val: (]"
  | Rparen_t -> "Token[type: rparen, val: )]"
;;

let rec parse_factor = function
  | Var_t x :: tl -> (Literal x, tl)
  | Neg_t :: tl ->
    let r = 
      match parse_factor tl with
      | (Literal x, l) -> (Literal (-x), l)
      | (res, l) -> (Not res, l)
    in
    r 
  | Lparen_t :: tl ->
    let (exp, l) = parse_expr tl in
    (match l with
    | Rparen_t :: tl -> (exp, tl)
    | _ -> raise (Failure "no closing parentheses"))
  | _ -> failwith "Error"
and parse_term ls =
  let (left, l) = parse_factor ls in
  match l with
  | And_t :: tl ->
    let (right, l) = parse_term tl in
    (Operation (And, left, right), l)
  | _ -> (left, l)
and parse_expr ls = 
  let (left, l) = parse_term ls in
  match l with
  | Or_t :: tl ->
    let (right, l) = parse_expr tl in
    (Operation (Or, left, right), l)
  | _ -> (left, l)

let print_op = function
    | And -> "And"
    | Or -> "Or"

let rec print_ast = function
    | Operation (o, l, r) ->
      printf "Operation %s [" (print_op o);
      print_ast l;
      print_ast r;
      printf "]"
    | Not e ->
      printf "- [";
      print_ast e;
      printf "]"
    | Literal i ->
      printf "Literal: %i;" i


(*
let _ =
  repl ()
;;*)