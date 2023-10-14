type 'a tree = Tip of 'a | Bin of 'a tree * 'a tree

let flatten input =
  let rec go input acc =
    match input with
    | Tip x -> x :: acc
    | Bin (left, right) -> go left (go right acc)
  in
  go input []

let fact n =
  let rec go n k = if n = 0 then k 1 else go (n - 1) (fun m -> k (n * m)) in
  go n Fun.id

let fact_defunc_1 n =
  let rec go input acc =
    match input with
    | 0 -> List.fold_left ( * ) 1 acc
    | n -> go (n - 1) (n :: acc)
  in
  go n []

let fact_defunc_2 n =
  let rec go input k = match input with 0 -> k | n -> go (n - 1) (n * k) in
  go n 1

(* quadratic *)
let rec reverse_slow = function [] -> [] | x :: xs -> reverse_slow xs @ [ x ]

let reverse_cps input =
  let rec go input k =
    match input with
    | [] -> k []
    | x :: xs -> go xs (fun xs_r -> k (xs_r @ [ x ]))
  in
  go input Fun.id

(* linear *)
let reverse_defunc input =
  let rec go input acc =
    match input with [] -> acc | x :: xs -> go xs (x :: acc)
  in
  go input []

(* defunction of the tree flattening function *)
let rec flatten_cps input k =
  match input with
  | Tip a -> k [ a ]
  | Bin (left, right) ->
      flatten_cps right (fun ys ->
          flatten_cps left (fun xs -> k (xs @ ys)))

(*
1. Left branch => \xs -> k (xs ++ ys) => ys
2. Right branch => \ys => flatten t (\xs => k (xs ++ ys)) => t
*)
type 'a flat_continuation = ('a list, 'a tree) Either.t
