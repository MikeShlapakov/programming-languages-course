type bool_exp = 
    | Var of string
    | Not of bool_exp
    | And of bool_exp * bool_exp
    | Or of bool_exp * bool_exp
;;

let rec insert_bool_val vars exp = match exp with
  | Var x -> List.assoc x vars
  | Not e -> not (insert_bool_val vars e)
  | And (e1, e2) -> (insert_bool_val vars e1) && (insert_bool_val vars e2)
  | Or (e1, e2) -> (insert_bool_val vars e1) || (insert_bool_val vars e2)
;;

let rec create_row vars = match vars with
  | [] -> [[]]
  | var :: rest -> let rest_row = create_row rest in
    List.concat [
      List.map (fun row -> (var, true) :: row) rest_row;
      List.map (fun row -> (var, false) :: row) rest_row
    ]
;;

let table vars exp =
  List.map (fun row -> (row, insert_bool_val row exp)) (create_row vars)
;;