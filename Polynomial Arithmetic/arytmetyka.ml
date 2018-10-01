(* Przechowujemy początek i koniec przedziału (Para a, b) *)
(* Dwa przypadki: 
   1. Przedzial normalny - <a, b>                ;  a < b
   2. Antyprzedzial      - (-inf, b> U <a, inf)  ;  b < a
*)
type wartosc = float * float;;

(* Epsilon do porównywań *)
let eps = 1.e-10;;

(* Funkcje pomocnicze *)

(* a  = b *)
let eps_eq (a : float) (b : float) : bool = ( ( abs_float (a -. b) ) < eps );;
(* a >= b *)
let eps_gt (a : float) (b : float) : bool = ( a -. b > -.eps );;

(* Sprawdzenie czy wartość jest nan-em *)
let is_nan (a : float) : bool = ( compare a nan = 0 );;

(* Sprawdzenie czy liczba jest rzeczywista *)
let is_real (a : float) : bool = (( a = infinity || a = neg_infinity || is_nan a ) = false );;

(* Sprawdzenie czy wartość jest dodatnia *)
let is_pos (a : float) : bool = eps_gt a 0.;;

(* Sprawdzenie czy przedział nie jest nan-em *)
let is_ok (a : float) (b : float) : bool = not (is_nan a || is_nan b);;

(* Sprawdzenie czy przedział jest "normalnej" (<a, b>) postaci *)
(* ( <a, b> czy (-inf, b> U <a, inf) ) *)
let is_norm (a : float) (b : float) : bool = eps_gt b a || eps_eq a b;;

(* Min i max niezwracające nan gdy któraś z liczb nie jest nan-em *)
let max_c (a : float) (b : float)  =
  if is_nan a then b else 
  if is_nan b then a else 
  if a < b    then b else a
;;

let min_c (a : float) (b : float)  =
  if is_nan a then b else
  if is_nan b then a else
  if a < b    then a else b
;;

(* Funkcje tworzące przedziały *)

let wartosc_dokladnosc (x : float) (p : float) : wartosc = 
  let niepew = abs_float(x *. (p /. 100.)) in 
    (x -. niepew, x +. niepew)
;;

let wartosc_od_do (poczatek : float) (koniec : float) : wartosc = (poczatek, koniec);;

let wartosc_dokladna (x : float) : wartosc = (x, x);;

(* Funkcje sprawdzające wartości przedziałów *)

let in_wartosc ((a,b) : wartosc) (x : float) : bool =
  if is_ok a b then 
    if is_norm a b then ( eps_gt x a && eps_gt b x ) else ( eps_gt x a || eps_gt b x )
  else false
;;

let min_wartosc ((a,b) : wartosc) : float = 
  if is_ok a b then 
    if is_norm a b then a else neg_infinity
  else nan
;;

let max_wartosc ((a,b) : wartosc) : float = 
  if is_ok a b then 
    if is_norm a b then b else infinity
  else nan
;;

let sr_wartosc ((a,b) : wartosc) : float = 
  if (*is_real a && is_real b &&*) is_ok a b && is_norm a b then
    (a +. b) /. 2.
  else nan
;;


(* Operacje na przedziałach *)

let rec plus ((a, b) : wartosc) ((c, d) : wartosc) : wartosc =
  if not (is_ok a b && is_ok c d) then (nan, nan) 
  else if is_norm a b = false && is_norm c d = false then
    (* 2 przedziały odwrócone *)
    (* istnieje d.d k, k+(-k+x) = x dla dowolnego x *)
    (neg_infinity, infinity)
  else if is_norm a b = false && is_norm c d = true  then
    plus (c, d) (a, b)       (* 1 przedział odwrócony - przypadek poniżej *)
  else if is_norm a b = true  && is_norm c d = false then
    (* <a, b> + (-inf, d> U <c, inf) , 1 przedział odwrócony, Zał. a < b, c > d *)
    if eps_gt (d +. b) (c +. a) then
      (* Jeżeli d+b >= c+a wtedy lewa i prawa strona nowego *)
      (* przedziału odwróconego się pokrywają - wynikiem są l. rzeczywiste *)
      (neg_infinity, infinity) 
    else (* W przeciwnym wypadku, wynikiem jest przedział powiększony
            zał. a < b, wynik - (-inf, d+b> U <c+a, inf)*)
      (c +. a, d +. b)
  else  (* 2 normalne przedziały, założenie a < b , c < d *)
        (* Zatem, wynikiem będzie <a+c, b+d>              *)
    (a +. c, b +. d) 
;;

let minus ((a, b) : wartosc) ((c, d) : wartosc) : wartosc =
  (* Dodawanie, tylko że przedziału przeciwnego *)
  plus (a, b) (-.d, -.c)
;;

let rec razy ((a, b) as x : wartosc) ((c, d) as y : wartosc) : wartosc =
  if not (is_ok a b && is_ok c d)                                 then (nan, nan) else
  if (eps_eq a 0. && eps_eq b 0.) || (eps_eq c 0. && eps_eq d 0.) then (0., 0.)   else 
  if is_norm a b = false && is_norm c d = false then
    (* Przypadek 2 przedziałów odwróconych - (-inf, b> U <a, inf) * (-inf, d> U <c, inf) *)
    let lhs = min_c (c *. a) (d *. b)
    and rhs = max_c (c *. b) (d *. a) in 
      if in_wartosc x 0. || in_wartosc y 0. then (neg_infinity, infinity) else (lhs, rhs)
  else if is_norm a b = false && is_norm c d = true  then
    (* Przypadek (-inf, b> U <a, inf) * <c, d> zamieniamy na równoważny *)
    razy (c, d) (a, b) 
  else if is_norm a b = true  && is_norm c d = false then
    (* Przypadek <a, b> * (-inf, d> U <c, inf) dzieli się na podprzypadki *)
    if in_wartosc x 0. then (neg_infinity, infinity) else 
    if in_wartosc y 0. then
      if eps_eq d 0. then                                                           (* <a, b> * (-inf, 0> U <c, inf) *)
        if is_pos a then ( a *. c , 0. ) else ( 0. , c *. b )
      else if eps_eq c 0. then                                                      (* <a, b> * (-inf, d> U <0, inf) *)
        if is_pos a then ( 0. , d *. a ) else ( d *. b , 0. )
      else if is_pos d then                                         (* <a, b> * (-inf, 0, d/np. 5> U <c/np. 10, inf) *)
        if is_pos a then
          if eps_gt (d *. b) (a *. c) then (neg_infinity, infinity) else (a *. c, b *. d)
        else
          if eps_gt (b *. c) (d *. a) then (neg_infinity, infinity) else (d *. a, c *. b)
      else                                                         (* <a, b> * (-inf, d/np. -5> U <c/np. -2, 0, inf) *)
        if is_pos a then
          if eps_gt (a *. d) (b *. c) then (neg_infinity, infinity) else (c *. b, a *. d)
        else 
          if eps_gt (a *. c) (d *. b) then (neg_infinity, infinity) else (d *. b, c *. a)
    else                                                               (* <a, b> * (-inf, d/np. -5> U <c/np. 5, inf) *)
      if is_pos a then (a *. c, a *. d) else (b *. d, b *. c)
  else 
    (* 2 przedziały normalne - (<a, b> * <c, d>) *)
    let lhs = min_c (min_c (c *. a) (c *. b)) (min_c (d *. a) (d *. b)) 
    and rhs = max_c (max_c (c *. a) (c *. b)) (max_c (d *. a) (d *. b)) in (lhs, rhs)
;;

(* Funkcja pomocnicza zwracajaca odwrotnosc przedzialu *)
let odwroc ((a, b) as x : wartosc) : wartosc = 
  (* Jeżeli przedział jest nan-em lub zerem, zwracamy nan *)
  if is_nan a || is_nan b || (eps_eq a 0. && eps_eq b 0.) then (nan, nan) else 
  (* Jeżeli przedział jest (-inf, inf) zwracamy (-inf. inf) *)
  if a = neg_infinity && b = infinity then (neg_infinity, infinity) else 
    (* W przeciwnym wypadku, rozpatrujemy przypadki... *)
    if is_norm a b then (* Normalny przedział, w formie <a, b> *)
      if in_wartosc x 0. then (* Jeżeli zawiera zero, to sprawdzamy czy jest ono na którymś z krańców *)
        if eps_eq a 0. then (1./.b, infinity)      else
        if eps_eq b 0. then (neg_infinity, 1./.a)  else
          (* Jeżeli nie jest, wyjdzie przedział postaci (-inf, 1/a> U <1/b, inf) *)
          (1./.b, 1./.a)
      else (* Jeżeli przedział nie zawiera zera, zwracamy normalny wynik postaci <1/b, 1/a> *)
        (1./.b, 1./.a)
    else (* Nie normalny przedział - w formie (-inf, b> U <a, inf) *)
      if in_wartosc x 0. then (* Jeżeli przedział zawiera zero, sprawdzamy przypadki krańcowe *)
        if eps_eq a 0. then (1./.b, infinity)     else
        if eps_eq b 0. then (neg_infinity, 1./.a) else 
          (* Jeżeli nie jest, zwracamy przedział postaci (-inf, 1/a> U <1/b, inf) *)
          (* Przypadki: (-inf, 0, b> U <a, inf) albo (-inf, b> U <a, 0, inf) *)
          (1./.b, 1./.a)
      else (* Jeżeli nie zawiera zera, zwracamy normalny przedział postaci <1/b, 1/a> *) 
        (1./.b, 1./.a)
;;

let podzielic ((a, b) as x : wartosc) ((c, d) as y : wartosc) : wartosc =
  if ( is_ok a b && is_ok c d ) then
    (* Dzielenie przedziału to mnożenie przez jego odwrotność *)
    razy x (odwroc y)
  else (nan, nan)
;;
