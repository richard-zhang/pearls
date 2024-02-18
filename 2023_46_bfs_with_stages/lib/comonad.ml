(*
Motivation: Why I want to study comonad?
Question: the paper: programming with phases uses applicative functor as the example for the
The particular categorical construct can also combine different comonad together (not monad). 
I want to study the comonad to see what interested result I can get from extending applicative functor to comonad
*)

(*
Blog: Sequences, streams, and segments   

1. continuous and infinite: function
2. discrete   and infinite: stream 
3. discrete   and finite  : sequence
4. continuous and finite  : ??

*)
open Bfs

module type ComonadSig = sig
  type 'a t

  include FunctorSig with type 'a t := 'a t

  val extract : 'a t -> 'a
  val duplicate : 'a t -> 'a t t
end

module Stream = struct
  type 'a t = Cons : 'a * (unit -> 'a t) -> 'a t

  let rec ( <$> ) f a =
    match a with Cons (a, rest) -> Cons (f a, fun () -> f <$> rest ())

  let rec pure a = Cons (a, fun () -> pure a)

  let rec ( <*> ) f a =
    match (f, a) with
    | Cons (f, f_rest), Cons (a, a_rest) ->
        Cons (f a, fun () -> f_rest () <*> a_rest ())

  let head = function Cons (x, _) -> x
  let tail = function Cons (_, rest) -> rest
  let rec tails : 'a t -> 'a t t = fun x -> Cons (x, fun () -> tails (tail x ()))
  let extract = head
  let duplicate = tails
end

module ComonadHelper (W : ComonadSig) = struct
  open W

  let extend (f : 'a t -> 'b) (w : 'a t) : 'b t =
    let dup_w = W.duplicate w in
    f <$> dup_w

  let ( =>> ) w f = extend f w
end

let%test_unit "test stream pure" =
  let open Stream in
  let value = pure 1 |> duplicate |> extract |> extract in
  Printf.printf "value is %d" value

module type SegmentSig = sig
  type 'a seg
  type len

  val length : 'a seg -> len
  val drop : len -> 'a seg -> 'a seg
  val take : len -> 'a seg -> 'a seg
end

module type Arg = sig
  type t
end

module FunctionMonad (Arg : Arg) : MonadSig with type 'a t = Arg.t -> 'a =
struct
  type 'a t = Arg.t -> 'a

  let ( <$> ) f v arg = arg |> v |> f
  let pure a _ = a
  let ( <*> ) f g arg = (f arg) (g arg)
  let ( let* ) a k arg = (k (a arg)) arg
end

(*
when the arg type is of monoid, some thing special will happen   
*)

module FunctionComonad (M : MonoidType) : ComonadSig = struct
  include FunctionMonad (M)

  let extract f = f M.empty
  let duplicate f l r = f (M.concat l r)
end

(*
Function provides a setting for generalized streams   
*)

(* Celluar Automa and Comonad *)

(* infite list in ocaml *)
type 'a infinite_list =
  | Cons : 'a * (unit -> 'a infinite_list) -> 'a infinite_list

type 'a u = U : 'a infinite_list * 'a * 'a infinite_list -> 'a u

let right = function
  | U (a, b, Cons (c, cs)) -> U (Cons (b, fun () -> a), c, cs ())

let left = function
  | U (Cons (x, xs), b, c) -> U (xs (), x, Cons (b, fun () -> c))

let rec iterate f seed = Cons (seed, fun () -> iterate f (f seed))

let rec map f = function
  | Cons (value, rest) -> Cons (f value, fun () -> map f (rest ()))

let tail = function Cons (_, rest) -> rest ()
let head = function Cons (a, _) -> a
let repeat a = iterate Fun.id a
let rec index i list = if i = 0 then head list else index (i - 1) (tail list)
let shift i u = index (Int.abs i) (iterate (if i < 0 then left else right) u)
let rec take i u = if i = 0 then [] else head u :: take (i - 1) (tail u)
let half = function U (_, b, c) -> Cons (b, fun () -> c)
let to_list i j u = u |> shift i |> half |> take (j - i)

module U : ComonadSig with type 'a t = 'a u = struct
  type 'a t = 'a u

  let ( <$> ) f = function
    | U (left, focus, right) -> U (map f left, f focus, map f right)

  let extract = function U (_, focus, _) -> focus

  (*
    duplicate is a universe of universe    
  *)
  let duplicate a = U (a |> iterate left |> tail, a, a |> iterate right |> tail)
end

(* Motivation: write rules that work on local neighbourhoods of our universe *)

let rule = function
  | U (left_neighbors, b, right_neighbors) ->
      let a = head left_neighbors in
      let c = head right_neighbors in
      not ((a && b && not c) || a = b)

let%test_unit "celluar automa\n" =
  let u = U (repeat false, true, repeat false) in
  let open ComonadHelper (U) in
  let f x = x =>> rule in
  u |> iterate f
  |> map (fun x ->
         x |> to_list (-40) 40
         |> List.map (fun x -> if x then '#' else ' ')
         |> List.to_seq |> String.of_seq)
  |> take 40 |> List.iter print_endline
