(*
  Y = \f. (\x. f (x x)) (\x. f (x x))   
    = \f. f ((\x. f (x x)) (\x. f (x x)))
    = \f. f ((\x. f (x x)) (\x. f (x x)))
    = \f. f (f ((\x. f (x x)) (\x. f (x x)))
    = \f. f (f (f (f ... ((\x. f (x x)) (\x. f (x x))))))
*)
let fac_go = fun f -> fun x -> if x = 0 then 1 else x * f (x - 1)

let fac_tail_go = fun f -> (fun acc x -> if x = 0 then acc else f (acc * x) (x - 1))
(* 


let fix f = (fun x a -> f (x x) a) (fun x a -> f (x x) a)

(* delaying the computation *)

let y f = (fun x -> f (x x)) (fun x -> f (x x)) *)

(* let z = fun f -> (fun x -> f (fun y -> (x x) y)) (fun x -> f (fun y -> (x x) y))  *)

let rec z_1 = fun f -> f (fun x -> (z_1 f) x)

let fact = z_1 fac_go

let fact_tail = z_1 fac_tail_go 1


let () = print_int (fact_tail 5)