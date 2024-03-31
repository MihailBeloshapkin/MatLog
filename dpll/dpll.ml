open Base
open Stdio

type cnf_info =
  { var_cnt : int
  ; cls_cnt : int
  }

type formula =
  { cnf_options : cnf_info
  ; clauses : int list list
  }

type cnf_result =
  | Unsat
  | Sat of int list

module CNFFormula = struct
  type t = formula

  let is_empty f =
    match f.clauses with
    | [] -> true
    | _ -> false
  ;;

  let contains_empty_clause f =
    let rec helper = function
      | [] -> false
      | [] :: _ -> true
      | _ :: tl -> helper tl 
    in
    helper f.clauses
  ;;

  let find_units_pures f =
    let pures = Hash_set.create ~size:f.cnf_options.var_cnt (module Int) in
    let all = Hash_set.create ~size:f.cnf_options.var_cnt (module Int) in
    let units = Hash_set.create ~size:f.cnf_options.cls_cnt (module Int) in
    let () =
      List.iter f.clauses ~f:(fun cl ->
        let () =
          List.iter cl ~f:(fun l ->
            let () =
              match Hash_set.mem pures (-l) with
              | true -> Hash_set.remove pures (-l)
              | false when not (Hash_set.mem all l) -> Hash_set.add pures l
              | _ -> ()
            in
            Hash_set.add all l)
        in
        match cl with
        | [ l ]
          when (not (Hash_set.mem pures l))
               && (not (Hash_set.mem units l))
               && not (Hash_set.mem units (-l)) -> Hash_set.strict_add_exn units l
        | _ -> ())
    in
    units, pures
  ;;

  let eliminate_pures f pures =
    { f with
      clauses =
        List.rev_filter f.clauses ~f:(fun cl ->
          not (List.exists cl ~f:(fun l -> Hash_set.mem pures l)))
    }
  ;;

  let substitute f lit =
    { f with
      clauses =
        List.rev_filter_map f.clauses ~f:(fun cl ->
          let exception Found of int in
          try
            Some
              (List.rev_filter cl ~f:(fun l ->
                 if l = lit then raise (Found l) else not (l = -lit)))
          with
          | Found _ -> None)
    }
  ;;

  let unit_propagation f units =
    { f with
      clauses =
        List.rev_filter_map f.clauses ~f:(fun cl ->
          let exception Found of int in
          try
            Some
              (List.rev_filter cl ~f:(fun l ->
                 if Hash_set.mem units l
                 then raise (Found l)
                 else not (Hash_set.mem units (-l))))
          with
          | Found _ -> None)
    }
  ;;

  let choose f = List.hd_exn @@ List.hd_exn f.clauses
end

let solve f =
  let exception Sat_exn of int list in
  let exception Unsat_exn in

  let rec loop f acc : cnf_result =
    let rec simplify f acc =
      if CNFFormula.is_empty f
      then raise (Sat_exn acc)
      else if CNFFormula.contains_empty_clause f
      then raise Unsat_exn
      else (
        let units, pures = CNFFormula.find_units_pures f in
        match Hash_set.is_empty units, Hash_set.is_empty pures with
        | false, false ->
          let f = CNFFormula.unit_propagation f units in
          simplify
            (CNFFormula.eliminate_pures f pures)
            (Hash_set.to_list units @ Hash_set.to_list pures @ acc)
        | false, true ->
          simplify (CNFFormula.unit_propagation f units) (Hash_set.to_list units @ acc)
        | true, false ->
          simplify (CNFFormula.eliminate_pures f pures) (Hash_set.to_list pures @ acc)
        | true, true -> f, acc)
    in
    match simplify f acc with
    | exception Sat_exn res -> Sat res
    | exception Unsat_exn -> Unsat
    | f, acc ->
      if CNFFormula.is_empty f
      then Sat acc
      else if CNFFormula.contains_empty_clause f
      then Unsat
      else (
        let l = CNFFormula.choose f in
        match loop (CNFFormula.substitute f l) (l :: acc) with
        | Unsat -> loop (CNFFormula.substitute f (-l)) (-l :: acc)
        | Sat acc -> Sat acc)
  in
  loop f []
;;