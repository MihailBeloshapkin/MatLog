open Parser
open Tseitin
open Dpll
open Stdio

(* repl *)
let print_prompt () =
  printf "|> ";
  Out_channel.flush stdout

let rec repl () =
  print_prompt ();
  let inp = In_channel.input_line stdin in
  match inp with
  | None -> printf "error reading line";
  | Some str ->
    let tokens = get_tokens str in
    match tokens with
    | [] -> printf ""; repl ()
    | _ -> 
      let (root, l) = parse_expr tokens in
      (*print_ast root;*)

      (** Получаем CNF преобразованием Цейтина *)
      let var_l, cnf = transform_tseitin root in
      printf "    After Tseitin transform:\n";
      print_acc cnf;
      let cnum = List.length cnf in
      let vnum = List.length var_l in 
      
      (** Применяем DPLL *)
      let result = solve { cnf_options = { var_cnt = vnum; cls_cnt = cnum }; clauses = cnf} in
      let () = match result with
      | Sat model -> 
        printf "\n    SAT\n    ";
        List.iter (printf "%i ") model;
      | Unsat -> printf "\n    UNSAT\n"
      in
      printf "\n";
      repl ()
;;

let _ =
  repl ()