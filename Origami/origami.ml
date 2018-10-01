type point  = float * float 
type kartka = point -> int  

let eps                                   = 1.e-10                   (* 1^-10  *)
let eps_eq (a : float) (b : float) : bool = abs_float (a -. b) < eps (* a  = b *)
let eps_gt (a : float) (b : float) : bool = a -. b > -.eps           (* a >= b *)

let kolko ((x1,y1) : point) (r : float) : kartka = fun ((x,y) : point) -> 
  if eps_gt (r *. r) ((x1 -. x) *. (x1 -. x) +. (y1 -. y) *. (y1 -. y)) then 1 else 0

let prostokat ((x1,y1) : point) ((x2,y2) : point) : kartka = fun ((x,y) : point) -> 
  if eps_gt x x1 && eps_gt x2 x && eps_gt y y1 && eps_gt y2 y then 1 else 0

(* Funkcja sprawdzająca po której stronie prostej leży dany punkt                       *)
(* 0 - punkt na prostej, wynik ujemny - punkt po lewej, wynik dodatni - punkt po prawej *)
let det ((x1,y1) : point) ((x2,y2) : point) ((x,y) : point) : float =
  (x -. x1) *. (y2 -. y1) -. (x2 -. x1) *. (y -. y1)

(* Funkcja odbijająca punkt względem danej prostej *)
let odbij ((x1,y1) : point) ((x2,y2) : point) ((x,y) : point) : point = 
  if x1 = x2 then (2. *. x1 -. x, y)  else (* Prosta x = liczba *)
  if y1 = y2 then (x, 2. *. y1 -. y)  else (* Prosta y = liczba *)
    let a  = (y2 -. y1) /. (x2 -. x1)   in (* Prosta p postaci y = ax + b      *)
    let b  = y1 -. a *. x1              in
    let ap = -.1. /. a                  in (* Prosta p' prostopadła do p       *)
    let bp = y -. ap *. x               in (*   przechodząca przez punkt (x,y) *)
    let xs = (bp -. b) /. (a -. ap)     in (* Punkt przecięcia prostych p i p' *)
    let ys = ap *. xs +. bp             in
    (2. *. xs -. x, 2. *. ys -. y)

let zloz (p1 : point) (p2 : point) (k : kartka) : kartka = fun (p : point) -> 
  let d = det p1 p2 p in
    if eps_eq d 0. then k p else 
    if d > 0.      then 0   else k p + k (odbij p1 p2 p)

let skladaj (l : (point * point) list) (k : kartka) : kartka = 
  List.fold_left (fun a (p1,p2) -> zloz p1 p2 a) k l
