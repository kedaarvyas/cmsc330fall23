(* tree structure *)
type 'a tree = 
  | Leaf 
  | Node of 'a tree * 'a * 'a tree

(* helpers *)
let rec map f xs = 
  match xs with
      [] -> []
    | h::t -> (f h)::(map f t)

let rec fold f a lst =
    match lst with
    []->a
    |h::t->fold f (f a h) t

let rec fold_right f lst a =
    match lst with
    []->a
    |h::t-> f h (fold_right f t a)

(* TODO *)
let rec fold_tree f b t = match t with
Leaf -> b
|Node(l, x, r) -> f (fold_tree f b l) x (fold_tree f b r)  

let rec map_tree f t = match t with
Leaf -> Leaf
|Node(l,x,r) -> Node((map_tree f l), (f x), (map_tree f r))

let add1 tree = map_tree (fun x -> x + 1) tree

let sum tree = fold_tree (fun l x r -> l + x + r) 0 tree
