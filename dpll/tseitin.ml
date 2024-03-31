open Str
open Stdio
open Dpll
open Parser
open Ast

(** Генерируем новую переменную *)
let generate_new_var var_list = 
  let max_var = var_list |> List.map abs |> List.fold_left max min_int in
  max_var + 1
;;

let unique_els l = List.fold_left (fun acc el -> if List.exists ((=) el) acc then acc else el ::acc ) [] l

(** Получаем список переменных из АСТ рекурсивным обходом *)
let rec get_all_vars_from_ast exp =
  match exp with
  | Literal l -> [ abs l ]
  | Operation (_, left, right) ->
    let left_vars = get_all_vars_from_ast left in
    let right_vars = get_all_vars_from_ast right in
    List.concat [ left_vars; right_vars ]
  | Not e -> get_all_vars_from_ast e
;;

let print_l acc =
  List.iter 
  (fun p -> printf "%i " p) 
  acc;
  printf "\n";
;;

(** Преобразоване Цейтина начинаем с самой внутренней подформулы, затем 
    движемся вверх по АСТ и применяем преобразование к вышестоящим узлам.
    Обход АСТ рекурсивный *)
let transform_tseitin exp =
  (** Параметры: выражение, список переменных, аккумулятор
      Возвращаем: новая переменная, новый список пременных, новый аккумулятор дизьюнктов  *)
  (** acc -- список дизьюнктов *)
  let rec cnf e var_list acc =
    (*printf "VARS:";
    print_l var_list;*)
    match e with
    | Operation (And, l, r) ->
      let l1, var_list1, acc1 = cnf l var_list acc in
      let l2, var_list2, acc2 = cnf r var_list1 acc1 in 
      let p = generate_new_var var_list2 in     
      printf "    [Log]: New Var: %i\n" p;

      let lacc = [ [ -p; l1]; [ -p; l2]; [ p; -l1; -l2 ] ] in
      p, p :: var_list2, List.concat [ lacc; acc2 ]
    | Operation (Or, l, r) ->
      let l1, var_list1, acc1 = cnf l var_list acc in
      let l2, var_list2, acc2 = cnf r var_list1 acc1 in 
      let p = generate_new_var var_list2 in     
      printf "    [Log]: New Var: %i\n" p;

      let lacc = [ [ p; -l1]; [ p; -l2]; [ -p; l1; l2 ] ] in
      p, p :: var_list2, List.concat [ lacc; acc2 ]
    | Literal l -> l, var_list, acc
    | Not e -> 
      let p, var_list1, acc1 = cnf e var_list acc in
      -p, (abs p) :: var_list1, acc1
  in
  let vars = exp |> get_all_vars_from_ast |> unique_els in
  let p, var_l, result = cnf exp vars [] in
  var_l, [ p ]  :: result
;;

let print_acc acc =
  List.iter 
    (fun dis ->
      printf "    ";
      List.iter 
        (fun p -> printf "%i " p) 
         dis;
      printf "\n"
    )
    acc
;;