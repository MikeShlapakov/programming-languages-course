type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree;;

let rec comparator_tree t v f = match t with
    | Empty -> Node(v, Empty, Empty)
    | Node(a, l, r) -> if f v a then Node(a, l, comparator_tree r v f) else Node(a, comparator_tree l v f, r)
;;