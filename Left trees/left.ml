type 'a queue = Node of 'a queue * 'a * int * 'a queue | Null;;
(* Czwórka (l, val, dst, r) :
   l   - lewe poddrzewo
   val - wartość przechowywana w węźle
   dst - prawa wysokosc poddrzewa
   r   - prawe poddrzewo
*)

exception Empty;;

let empty : 'a queue = Null;;

(* F. pomocnicza do wyciągania prawej wysokosci z czwórki *)
let dst (q : 'a queue) : int =
  match q with 
    | Null              -> 0 (* Null jest w odległości 0 od null-a *)
    | Node(_, _, d, _)  -> d
;;

let rec join (q1 : 'a queue) (q2 : 'a queue) : 'a queue = 
  match (q1, q2) with
    | (Null, q   ) -> q (* Jeżeli lewe jest null-em zwracamy prawe (jeżeli oba są null-em, zwracamy prawego null-a) *)
    | (q   , Null) -> q (* Jeżeli prawe jest null-em, zwracamy lewe (które nie będzie null-em) *)
    | (Node(l, e1, _, r), Node(_, e2, _, _)) -> 
        (* Jeżeli element w korzeniu lewego poddrzewa jest większy (ma niższy priorytet) od elementu w korzeniu prawego poddrzewa, 
           zamieniamy drzewa miejscami *)
        if e1 > e2 then join q2 q1 else
          (* Stwórz nowe, na razie prawe poddrzewo *)
          let ret = join r q2 in
            (* Zamiana poddrzew, żeby zachować warunek lewicowości *)
            if dst l < dst ret then 
              Node(ret, e1, dst l   + 1, l  )
            else
              Node(l  , e1, dst ret + 1, ret)
;;

let is_empty (q : 'a queue) : bool = (q = empty);; 

let add (e : 'a) (q : 'a queue) : 'a queue = 
  join q (Node(Null, e, 1, Null)) (* Nowy element leży w odległości 1 od null-a *)
;;

let delete_min (q : 'a queue) : ('a * 'a queue) =
  match q with
    | Null              ->  raise Empty
    | Node(l, e, _, r)  ->  (e, join l r)
;;
