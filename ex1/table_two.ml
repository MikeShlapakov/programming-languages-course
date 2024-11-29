type bool_exp = 
    | Var of string
    | Not of bool_exp
    | And of bool_exp * bool_exp
    | Or of bool_exp * bool_exp
;;

let rec insert_bool_val a b exp a_val b_val = match exp with
    | Var s -> if s = a then a_val else b_val
    | Not p -> not (insert_bool_val a b p a_val b_val)
    | And (p, q) -> (insert_bool_val a b p a_val b_val) && (insert_bool_val a b q a_val b_val)
    | Or (p, q) -> (insert_bool_val a b p a_val b_val) || (insert_bool_val a b q a_val b_val)
;;

let table_two a b exp = List.map (fun (a_val, b_val) -> (a_val, b_val, insert_bool_val a b exp a_val b_val)) 
    [(true, true); 
    (true, false); 
    (false, true); 
    (false, false)]
;;
