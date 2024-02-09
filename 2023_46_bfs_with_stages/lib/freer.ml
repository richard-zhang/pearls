(* once again I'm writing an expression tree :) *)
(*
Why do I want to use freer moand?   
1. I want it to be applicative???
2. technically I don't need to since I didn't even implement applicative functor for tree
*)

open Bfs

module FreerMonad (Op : sig
  type 'a op
end) =
struct
  include Op

  type 'a t = Return : 'a -> 'a t | Do : 'b op * ('b -> 'a t) -> 'a t

  let pure v = Return v

  let rec ( <$> ) : type a b. (a -> b) -> a t -> b t =
   fun f t ->
    match t with
    | Return v -> Return (f v)
    | Do (op, cont) -> Do (op, fun v -> f <$> cont v)

  let rec ( <*> ) : type a b. (a -> b) t -> a t -> b t =
   fun f t ->
    match f with
    | Return f -> f <$> t
    | Do (f, cont) -> Do (f, fun b -> cont b <*> t)

  let rec ( let* ) : type a b. a t -> (a -> b t) -> b t =
   fun t k ->
    match t with
    | Return v -> k v
    | Do (op, cont) ->
        Do
          ( op,
            fun v ->
              let* a = cont v in
              k a )

  let to_freer : 'a op -> 'a t = fun a -> Do (a, fun x -> Return x)

  module InterpFreer (M : MonadSig) = struct
    (* explicit universally quantified types in ocaml, for all x, f a -> m a*)
    type handler = { f : 'a. 'a op -> 'a M.t }

    let rec interp_freer : handler -> 'a t -> 'a M.t =
     fun { f } t ->
      match t with
      | Return v -> M.pure v
      | Do (e, cont) ->
          let open M in
          let* v = f e in
          v |> cont |> interp_freer { f }
  end
end

(* Shall I just use system F? *)
module Console = struct
  module Op = struct
    type _ op =
      | PutStrLn : string -> unit op
      | GetLine : string op
      | ExitSuccess : unit op
  end

  include FreerMonad (Op)

  let putstrln str = to_freer (PutStrLn str)
  let getline () = to_freer GetLine
  let exit () = to_freer ExitSuccess
end

let f : type a. a Console.op -> unit -> a = function
  | PutStrLn str -> fun () -> print_endline str
  | GetLine -> fun () -> read_line ()
  | ExitSuccess -> fun () -> exit 0

let example_program =
  let open Console in
  let* _ = putstrln "Input Your Name:" in
  let* name = getline () in
  let* _ = putstrln "Input Your Age:" in
  let* age = getline () in
  let format_str =
    Printf.sprintf "My name is %s and I'm %s years old" name age
  in
  let* _ = putstrln format_str in
  exit ()

let run () =
  let open Console.InterpFreer (DelayIdentityFunctor) in
  let handler = { f } in
  interp_freer handler example_program ()

module CompGraph = struct
  module Op = struct
    (* type 'a op =
       | Var of string
       | Const of float
       | Sin of 'a
       | Plus of 'a * 'a
       | Mul of 'a * 'a *)

    type _ op =
      | Var : string -> float op
      | Const : float -> float op
      | Sin : float -> float op
      | Plus : (float * float) -> float op
      | Mul : (float * float) -> float op
  end

  include FreerMonad (Op)

  (* x sin(x) + 2 * x *)
  let var x = to_freer (Var x)
  let const v = to_freer (Const v)

  let sin v =
    let* x = v in
    to_freer (Sin x)

  let plus a b =
    let* a = a in
    let* b = b in
    to_freer (Plus (a, b))

  let mul a b =
    let* a = a in
    let* b = b in
    to_freer (Mul (a, b))
end

module StringMap = Map.Make (struct
  type t = string

  let compare = String.compare
end)

type env = float StringMap.t

(* x sin(x) + 2 * x *)
let example x =
  let open CompGraph in
  let var = var x in
  plus (mul var (sin var)) (mul (const 2.) var)

module M = CompGraph.InterpFreer (IdentityFunctor)

let handler (env : env) : M.handler =
  let f : type a. a CompGraph.op -> a = function
    | Var v -> StringMap.find v env
    | Const x -> x
    | Sin x -> sin x
    | Plus (a, b) -> a +. b
    | Mul (a, b) -> a *. b
  in
  M.{ f }

let%test_unit "run the example function" =
  let env = StringMap.of_seq (List.to_seq [ ("x", 3.0) ]) in
  let a = M.interp_freer (handler env) (example "x") in
  Printf.printf "\n%0.3f\n" a
