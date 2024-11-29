let insert_at v n arr =
    let rec aux i list = match list with 
        | [] -> if n = i then v::[] else []
        | h::t -> if n = i then v::h::t else h::aux (i+1) t
    in aux 0 arr;;