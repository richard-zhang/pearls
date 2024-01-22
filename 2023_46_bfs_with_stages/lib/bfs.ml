(* Intro
   - Why pick this talk?
   - Phases in Software Architecture
*)

module type ApplicativeFunctorSig = sig
  type 'a t

  val pure : 'a -> 'a t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end

(* #tech: need to exposed the abstract type definition *)
module IdentityFunctor : ApplicativeFunctorSig with type 'a t = 'a = struct
  type 'a t = 'a

  let pure x = x
  let ( <$> ) f = f
  let ( <*> ) f a = f a
end

module DelayIdentityFunctor :
  ApplicativeFunctorSig with type 'a t = unit -> 'a = struct
  type 'a t = unit -> 'a

  let pure x () = x
  let ( <$> ) f a () = f (a ())

  let ( <*> ) f a () =
    let f' = f () in
    let a' = a () in
    f' a'
end

module ListFunctor : ApplicativeFunctorSig with type 'a t = 'a list = struct
  type 'a t = 'a list

  let pure x = [ x ]
  let rec ( <$> ) f = function [] -> [] | x :: xs -> f x :: (f <$> xs)

  let rec ( <*> ) funcs elems =
    match (funcs, elems) with
    | f :: fs, e :: es -> f e :: (fs <*> es)
    | [], _ -> []
    | _, [] -> []
end

module Helper (F : ApplicativeFunctorSig) = struct
  open F

  let unit = F.pure ()
  let cross a b = (fun x y -> (x, y)) <$> a <*> b
  let ( *> ) ma mb = snd <$> cross ma mb
end

(* Applicative Traversal *)

(* Interface of a traversal: need contain a applicative defintion representing the effect of each individual travere
   and a container type
*)
module type TraversalSig = sig
  type 'a t
  type 'a f

  val traverse : ('a -> 'b f) -> 'a t -> 'b t f
end

module ListTraversal : functor (F : ApplicativeFunctorSig) ->
  TraversalSig with type 'a f = 'a F.t and type 'a t = 'a list =
functor
  (F : ApplicativeFunctorSig)
  ->
  struct
    type 'a f = 'a F.t
    type 'a t = 'a list

    open F

    let rec traverse f = function
      | [] -> pure []
      | x :: xs -> List.cons <$> f x <*> traverse f xs
  end

(*
Well-behaved traversals are those satisfying three axioms of naturality, linearity, and unitarity Natural
*)

(* forest a = [tree a] *)
type 'a tree = Node of ('a * 'a tree list)

module TreeFunctor : ApplicativeFunctorSig with type 'a t = 'a tree = struct
  type 'a t = 'a tree

  let pure x = Node (x, [])

  let rec ( <$> ) f = function
    | Node (value, forest) ->
        Node (f value, ListFunctor.( <$> ) (( <$> ) f) forest)

  (* shpae must be the same *)
  let rec ( <*> ) f_tree tree =
    match (f_tree, tree) with
    | Node (f, f_forest), Node (t, t_forest) ->
        let tem = ListFunctor.( <$> ) ( <*> ) f_forest in
        Node (f t, ListFunctor.( <*> ) tem t_forest)
end

(*
3 - 1 - 1 
 \   \    
  \   5   
   \
    4 - 9
     \
      2
*)

let node a b = Node (a, b)

let t =
  node 3 [ node 1 [ node 1 []; node 5 [] ]; node 4 [ node 9 []; node 2 [] ] ]

(* depth first traversl *)

module DfsTreeTraversal : functor (F : ApplicativeFunctorSig) ->
  TraversalSig with type 'a f = 'a F.t and type 'a t = 'a tree =
functor
  (F : ApplicativeFunctorSig)
  ->
  struct
    module ListTraverse = ListTraversal (F)

    type 'a f = 'a F.t
    type 'a t = 'a tree

    open F

    let rec traverse f = function
      | Node (x, kids) ->
          node <$> f x <*> ListTraverse.traverse (traverse f) kids
  end

let%test_unit "dfs tree traversal" =
  print_newline ();
  let f a = Printf.printf "visited %d\n" a in
  let module TreeTraversal = DfsTreeTraversal (IdentityFunctor) in
  let _ = TreeTraversal.traverse f t in
  ()

(* what about breath-first traversal *)

(* breath-first enumeration *)

(* first approach: level order traversal *)
let rec lzw f left right =
  match (left, right) with
  | x :: xs, y :: ys -> f x y :: lzw f xs ys
  | [], ys -> ys
  | xs, [] -> xs

let rec levels = function Node (x, ts) -> [ x ] :: levelsF ts
and levelsF kids = kids |> List.map levels |> List.fold_left (lzw ( @ )) []

(* breath first enumeration is defined to be *)
let bf tree = tree |> levels |> List.concat

let%test_unit "breath first enumeration" =
  print_newline ();
  t |> bf |> List.iter (fun i -> Printf.printf "%d," i)

let print_tree_by_levels t =
  let f i content =
    Printf.printf "level %d: " i;
    List.iter (Printf.printf " %d,") content;
    print_newline ()
  in
  t |> levels |> List.iteri f

let%test_unit "level order traversal of t" =
  print_newline ();
  print_tree_by_levels t

(* breath first enumeration is not reversible : one cannot reconstruct the input the function based on the output of the function *)

(* The missing information is the shape of the tree, i.e. tree with tuple as the value *)
let shape tree = TreeFunctor.( <$> ) (fun _ -> ()) tree

let rec relabel shape level_enumeration =
  match (shape, level_enumeration) with
  | Node ((), ts), (x :: xs) :: xss ->
      let us, yss = relabelF ts xss in
      (Node (x, us), xs :: yss)
  | _, _ -> failwith "level_enumeration didn't match the shape"

and relabelF forest xss =
  match forest with
  | [] -> ([], xss)
  | t :: ts ->
      let label_t, remain = relabel t xss in
      let rest, remain = relabelF ts remain in
      (label_t :: rest, remain)

(*
  A tree can be constructed by its level order traversal plus its shape
  A tree can also produce level order traversal and output its shape 
  This is a bijection definition. A <=> B
*)

let split t = (shape t, levels t)
let combine shape level_enumeration = relabel shape level_enumeration |> fst

(*
  Traversing on a list is very easy
  Problem solving technique: convert a hard problem into a problem we know how to solve
*)

module FirstPrincipleBfsTreeTraversal : functor (F : ApplicativeFunctorSig) ->
  TraversalSig with type 'a f = 'a F.t and type 'a t = 'a tree =
functor
  (F : ApplicativeFunctorSig)
  ->
  struct
    module ListTraverse = ListTraversal (F)
    (*
    (a      -> b      f) -> a      list -> b      list f
    (a list -> b list f) -> a list list -> b list list f
    *)

    type 'a f = 'a F.t
    type 'a t = 'a tree

    open F

    let traverse f tree =
      let shape, levels = split tree in
      let traverse_list = ListTraverse.traverse f in
      let traverse_level = ListTraverse.traverse traverse_list in
      let effect_traverse = traverse_level levels in
      let construct_tree_from_level = combine shape in
      construct_tree_from_level <$> effect_traverse
  end

let%test_unit "bfs tree trasersal \n" =
  let print i () = Printf.printf "%i, " i in
  let module Traverse = FirstPrincipleBfsTreeTraversal (DelayIdentityFunctor) in
  let a = Traverse.traverse print t in
  assert (a () = shape t)

(* repmin problem replace every element of a tree with the minimum element in that tree *)

(* first thought two pass algorithm *)
let repmin t =
  let rec minT = function
    | Node (x, []) -> x
    | Node (x, ts) ->
        min x
          (Base.List.min_elt ~compare:Int.compare (List.map minT ts)
          |> Option.get)
  in
  let rec replaceT y = function
    | Node (_, ts) -> node y (List.map (replaceT y) ts)
  in
  replaceT (minT t) t

let%test_unit "repmin: naive example" =
  print_newline ();
  t |> repmin |> print_tree_by_levels

let repmin_adv t =
  let rec auxT (t : int tree) : (int -> int tree) * int =
    match t with
    | Node (x, []) ->
        let traverser y = node y [] in
        (traverser, x)
    | Node (x, ts) ->
        let us, z = auxF ts in
        let traverser y = node y (us y) in
        (traverser, min z x)
  and auxF ts =
    let us, ys = ts |> List.map auxT |> Base.List.unzip in
    let global_min = Base.List.min_elt ~compare:Int.compare ys |> Option.get in
    let traverser y = List.map (fun u -> u y) us in
    (traverser, global_min)
  in
  let traverser, min = auxT t in
  traverser min

let%test_unit "repmin: advanced example" =
  print_newline ();
  t |> repmin_adv |> print_tree_by_levels

let unitl ((), a) = a
let unitr (a, ()) = a
let assoc (a, (b, c)) = ((a, b), c)
let twist (a, b) = (b, a)
let exch4 ((a, b), (c, d)) = ((a, c), (b, d))
let cross f g (x, y) = (f x, g y)

(* GADT *)
type t = E : 'a * ('a -> 'a) * ('a -> string) -> t

(* open is the pattern matching *)
(* pack is the data constructor *)

(* Day constructor need to be exposed to user, cannot be kept as abstracted *)

(* #tech: commcomposition of two functor with modules, again the ability to expose type is important *)
module DayFunctor =
functor
  (M : ApplicativeFunctorSig)
  (N : ApplicativeFunctorSig)
  ->
  struct
    type 'c t = Day : ('a * 'b -> 'c) * 'a M.t * 'b N.t -> 'c t
  end

module type ApplicativeFunctorWithPhase = functor
  (M : ApplicativeFunctorSig)
  (N : ApplicativeFunctorSig)
  -> sig
  type 'c t = Day : ('a * 'b -> 'c) * 'a M.t * 'b N.t -> 'c t

  include ApplicativeFunctorSig with type 'a t := 'a t

  val phase1 : 'a M.t -> 'a t
  val phase2 : 'a N.t -> 'a t
end

module Day : ApplicativeFunctorWithPhase =
functor
  (M : ApplicativeFunctorSig)
  (N : ApplicativeFunctorSig)
  ->
  struct
    (* include DayFunctor (M) (N) *)
    type 'c t = Day : ('a * 'b -> 'c) * 'a M.t * 'b N.t -> 'c t

    let unit = Day (unitr, M.pure (), N.pure ())

    let ( <$> ) f a =
      match a with Day (g, ma, nb) -> Day ((fun x -> x |> g |> f), ma, nb)

    let pure a = Fun.const a <$> unit

    module HelperM = Helper (M)
    module HelperN = Helper (N)

    let ( <*> ) dayf daya =
      match (dayf, daya) with
      | Day (f, ma, nb), Day (g, ma', nb') ->
          let m_pair = HelperM.cross ma ma' in
          let n_pair = HelperN.cross nb nb' in
          let h ((a, a_1), (b, b_1)) = (f (a, b)) (g (a_1, b_1)) in
          Day (h, m_pair, n_pair)

    let phase1 ma = Day ((fun (x, _) -> x), ma, HelperN.unit)
    let phase2 nb = Day ((fun (_, y) -> y), HelperM.unit, nb)
  end

module type ApplicativeFunctorWithCombinedPhase = functor
  (M : ApplicativeFunctorSig)
  -> sig
  include module type of Day (M) (M)

  val runDay : 'a t -> 'a M.t
end

module CombinedDay : ApplicativeFunctorWithCombinedPhase =
functor
  (M : ApplicativeFunctorSig)
  ->
  struct
    include Day (M) (M)
    module HelperM = Helper (M)

    let runDay (Day (f, ma, mb)) =
      let open M in
      (* run ma first and then run m b *)
      f <$> HelperM.cross ma mb
  end

module Example = CombinedDay (DelayIdentityFunctor)
module HelperExample = Helper (Example)

let%test_unit "running" =
  let open Example in
  let open HelperExample in
  print_newline ();
  (runDay
     (phase1 (fun () -> print_string "1")
     *> phase2 (fun () -> print_string "2")
     *> phase2 (fun () -> print_string "3")
     *> phase1 (fun () -> print_string "4")
     *> phase2 (fun () -> print_string "5")))
    ()

(* writer monad *)
module type MonoidType = sig
  type t

  val concat : t -> t -> t
  val empty : t
end

module type MakeWriterApplicativeFunctorSig = functor
  (Monoid : MonoidType)
  -> sig
  type 'a t = 'a * Monoid.t

  include ApplicativeFunctorSig with type 'a t := 'a t

  val write : Monoid.t -> unit t
  val runWriter : 'a t -> 'a * Monoid.t
end

module MakeWriterApplicativeFunctor : MakeWriterApplicativeFunctorSig =
functor
  (Monoid : MonoidType)
  ->
  struct
    type 'a t = 'a * Monoid.t

    let pure x = (x, Monoid.empty)
    let ( <$> ) f (a, w) = (f a, w)
    let ( <*> ) (f_writer, w1) (a, w2) = (f_writer a, Monoid.concat w1 w2)
    let write s = ((), s)
    let runWriter a = a
  end

(* reader monad *)

module type EnvType = sig
  type t
end

module type MakeReaderApplicativeFunctorSig = functor (Env : EnvType) -> sig
  type 'a t = Env.t -> 'a

  include ApplicativeFunctorSig with type 'a t := 'a t

  val ask : Env.t t
  val runReader : 'a t -> Env.t -> 'a
end

module MakeReaderApplicativeFunctor : MakeReaderApplicativeFunctorSig =
functor
  (Env : EnvType)
  ->
  struct
    type 'a t = Env.t -> 'a

    let pure = Fun.const
    let ( <$> ) f a env = f (a env)
    let ( <*> ) f a env = (f env) (a env)
    let ask = Fun.id
    let runReader = Fun.id
  end

module MinInt : sig
  type t = int

  include EnvType with type t := t
  include MonoidType with type t := t
end = struct
  type t = int

  let empty = Int.max_int
  let concat = Int.min
end

module WInt = MakeWriterApplicativeFunctor (MinInt)
module RInt = MakeReaderApplicativeFunctor (MinInt)
module WRIntDay = Day (WInt) (RInt)

let rec minAux (t : int tree) : unit WInt.t =
  let open WInt in
  let open Helper (WInt) in
  match t with
  | Node (value, []) -> write value
  | Node (value, x :: xs) ->
      let trees =
        xs |> List.map minAux
        |> List.fold_left (fun acc x -> acc *> x) (minAux x)
      in
      write value *> trees

let rec replaceAux : int tree -> int tree RInt.t = function
  | Node (_, trees) ->
      let open RInt in
      let open Helper (RInt) in
      let tree =
        trees |> List.map replaceAux
        |> Fun.flip
             (List.fold_right (fun a acc -> List.cons <$> a <*> acc))
             (pure [])
      in
      node <$> ask <*> tree

let repminAux (t : int tree) : int tree WRIntDay.t =
  let open WRIntDay in
  let open Helper (WRIntDay) in
  phase1 (minAux t) *> phase2 (replaceAux t)

let runWRintDay (t : 'a WRIntDay.t) : 'a =
  match t with
  | Day (f, wa, rb) ->
      let x, minimum = WInt.runWriter wa in
      let y = RInt.runReader rb minimum in
      f (x, y)

let%test_unit "repmin: day naive example" =
  print_newline ();
  t |> repminAux |> runWRintDay |> print_tree_by_levels

module WRIntTreeTraveral = DfsTreeTraversal (WRIntDay)

let repminDayAdv (t : int tree) : int tree =
  let open WRIntDay in
  let open Helper (WRIntDay) in
  t
  |> WRIntTreeTraveral.traverse (fun x ->
         phase1 (WInt.write x) *> phase2 RInt.ask)
  |> runWRintDay

let%test_unit "repmin: day naive example" =
  print_newline ();
  t |> repminDayAdv |> print_tree_by_levels

(* multi phases *)
