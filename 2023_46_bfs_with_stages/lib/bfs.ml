(* Intro
   - Why pick this talk?
   - Phases in Software Architecture
*)

module type FunctorSig = sig
  type 'a t

  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
end

module type ApplicativeFunctorSig = sig
  type 'a t

  val pure : 'a -> 'a t
  val ( <$> ) : ('a -> 'b) -> 'a t -> 'b t
  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
end

module type MonadSig = sig
  include ApplicativeFunctorSig

  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

(* #tech: need to exposed the abstract type definition *)
module IdentityFunctor : MonadSig with type 'a t = 'a = struct
  type 'a t = 'a

  let pure x = x
  let ( <$> ) f = f
  let ( <*> ) f a = f a
  let ( let* ) ma mab = mab ma
end

module DelayIdentityFunctor : MonadSig with type 'a t = unit -> 'a = struct
  type 'a t = unit -> 'a

  let pure x () = x
  let ( <$> ) f a () = f (a ())

  let ( <*> ) f a () =
    let f' = f () in
    let a' = a () in
    f' a'

  let ( let* ) ma mab () =
    let a = ma () in
    (mab a) ()
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
  let fmap = ( <$> )
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

(*
The signature is designed to satisfy the following requirement
1. functor
2. container type
3. various traverse implementation even upon on the same container type
4. expose 1 and 2
*)

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
and levelsF forest = forest |> List.map levels |> List.fold_left (lzw ( @ )) []

(* breath first enumeration is defined to be *)
let bf tree = tree |> levels |> List.concat

let%test_unit "breath first enumeration" =
  print_newline ();
  t |> bf |> List.iter (fun i -> Printf.printf "%d," i)

let print_tree_by_levels t =
  print_newline ();
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

let unzip list =
  List.fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) list ([], [])

(* one optimize strategy fusion: split is done by two traversals but we can fuse them into one traversal *)
let rec fuse_split = function
  | Node (x, ts) ->
      let forest, levels = fuse_split_f ts in
      (Node ((), forest), [ x ] :: levels)

and fuse_split_f forest =
  let forest, levels = forest |> List.map fuse_split |> unzip in
  (forest, List.fold_left (lzw ( @ )) [] levels)

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
      let shape, levels = fuse_split tree in
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

(*
Implement an efficient breath first traersal now becomes a problem of reducing 
the number of traversal on a tree
*)

(*
   A king of program:
   - circular definitions
   - fusing together multiple passes
  
   repmin is a typical problem
*)

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

(*
  Core insight: two traversal can be fused together
  traverse f t `cross` traverse g t = fmap unzip (traverse (fun x -> f x `cross` g x) t)
  Core insight:

  Fusing traversal with effect is usually not equivalent due to interleaving of effects.
*)

module type FirstOrderFunctorSig = functor (_ : Map.OrderedType) ->
  Map.OrderedType

module type SecondOrderFunctorSig = functor (_ : FirstOrderFunctorSig) ->
  Map.OrderedType

module SeondOrderModule : SecondOrderFunctorSig =
functor
  (FirstOrderFunctorSig : FirstOrderFunctorSig)
  ->
  struct
    include FirstOrderFunctorSig (Int)
  end

(* GADT *)
type t = E : 'a * ('a -> 'a) * ('a -> string) -> t

(* open is the pattern matching *)
(* pack is the data constructor *)

(* Day constructor need to be exposed to user, cannot be kept as abstracted *)

(* #tech: commcomposition of two functor with modules, again the ability to expose type is important *)
module DayFunctor =
functor
  (F : ApplicativeFunctorSig)
  (G : ApplicativeFunctorSig)
  ->
  struct
    type 'a f = 'a F.t
    type 'a g = 'a G.t
    type 'c t = Day : ('a * 'b -> 'c) * 'a f * 'b g -> 'c t
  end

module type DayApplicativeFunctorWithPhase = functor
  (F : ApplicativeFunctorSig)
  (G : ApplicativeFunctorSig)
  -> sig
  include module type of DayFunctor (F) (G)
  include ApplicativeFunctorSig with type 'a t := 'a t

  val phase1 : 'a f -> 'a t
  val phase2 : 'a g -> 'a t
end

module Day : DayApplicativeFunctorWithPhase =
functor
  (F : ApplicativeFunctorSig)
  (G : ApplicativeFunctorSig)
  ->
  struct
    include DayFunctor (F) (G)

    let unit = Day (unitr, F.pure (), G.pure ())

    let ( <$> ) f a =
      match a with Day (g, ma, nb) -> Day ((fun x -> x |> g |> f), ma, nb)

    let pure a = Fun.const a <$> unit

    module HelperF = Helper (F)
    module HelperG = Helper (G)

    let ( <*> ) dayf daya =
      match (dayf, daya) with
      | Day (f, ma, nb), Day (g, ma', nb') ->
          let m_pair = HelperF.cross ma ma' in
          let n_pair = HelperG.cross nb nb' in
          let h ((a, a_1), (b, b_1)) = (f (a, b)) (g (a_1, b_1)) in
          Day (h, m_pair, n_pair)

    let phase1 ma = Day ((fun (x, _) -> x), ma, HelperG.unit)
    let phase2 nb = Day ((fun (_, y) -> y), HelperF.unit, nb)
  end

module DayWithSameEffect =
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

module Example = DayWithSameEffect (DelayIdentityFunctor)
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

module type MakeWriterSig = functor (Monoid : MonoidType) -> sig
  type 'a t = 'a * Monoid.t

  include ApplicativeFunctorSig with type 'a t := 'a t

  val write : Monoid.t -> unit t
  val runWriter : 'a t -> 'a * Monoid.t
end

module MakeWriter : MakeWriterSig =
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

module type MakeReaderSig = functor (Env : EnvType) -> sig
  type 'a t = Env.t -> 'a

  include ApplicativeFunctorSig with type 'a t := 'a t

  val ask : Env.t t
  val runReader : 'a t -> Env.t -> 'a
end

module MakeReader : MakeReaderSig =
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

module WInt = MakeWriter (MinInt)
module RInt = MakeReader (MinInt)
module WRIntDay = Day (WInt) (RInt)

let rec min_aux (t : int tree) : unit WInt.t =
  let open WInt in
  let open Helper (WInt) in
  match t with
  | Node (value, forest) ->
      forest |> List.map min_aux
      |> List.fold_left (fun acc x -> acc *> x) (write value)

let min_aux_in_traverse : int tree -> unit tree WInt.t =
  let module M = DfsTreeTraversal (WInt) in
  M.traverse WInt.write

let rec replace_aux : int tree -> int tree RInt.t = function
  | Node (_, trees) ->
      let open RInt in
      let open Helper (RInt) in
      let tree =
        trees |> List.map replace_aux
        |> Fun.flip
             (List.fold_right (fun a acc -> List.cons <$> a <*> acc))
             (pure [])
      in
      node <$> ask <*> tree

let replace_aux_in_traverse : int tree -> int tree RInt.t =
  let module M = DfsTreeTraversal (RInt) in
  M.traverse (Fun.const RInt.ask)

let repmin_aux (t : int tree) : int tree WRIntDay.t =
  let open WRIntDay in
  let open Helper (WRIntDay) in
  phase1 (min_aux t) *> phase2 (replace_aux t)

let repmin_aux_in_traverse (t : int tree) : int tree WRIntDay.t =
  let open WRIntDay in
  let open Helper (WRIntDay) in
  phase1 (min_aux_in_traverse t) *> phase2 (replace_aux_in_traverse t)

let repmin_fusion =
  let open WRIntDay in
  let open Helper (WRIntDay) in
  let module M = DfsTreeTraversal (WRIntDay) in
  M.traverse (fun x -> phase1 (WInt.write x) *> phase2 RInt.ask)

(*
the original problem has a type signature int tree -> int tree
now, we have int tree -> int tree WRIntDay.t
We need int tree WRIntDay.t -> int tree
There're two ways parWR and seqWR
*)

let seqWR : 'a WRIntDay.t -> 'a = function
  | Day (f, writer, reader) ->
      let x, s = WInt.runWriter writer in
      let y = RInt.runReader reader s in
      f (x, y)

(*
some equational reasoning:

phase1 and phase 2 : a f -> a f*g or a g -> a f*g are
morphism in applicative functor (the category where the object are applicative functor)

phase1 (traverse f) = traverse (phase1 f)
*)

let runWRintDay (t : 'a WRIntDay.t) : 'a =
  match t with
  | Day (f, wa, rb) ->
      let x, minimum = WInt.runWriter wa in
      let y = RInt.runReader rb minimum in
      f (x, y)

let%test_unit "\nrepmin: day naive example 1\n" =
  t |> repmin_aux |> runWRintDay |> print_tree_by_levels

module WRIntTreeTraveral = DfsTreeTraversal (WRIntDay)

let repminDayAdv (t : int tree) : int tree =
  let open WRIntDay in
  let open Helper (WRIntDay) in
  t
  |> WRIntTreeTraveral.traverse (fun x ->
         phase1 (WInt.write x) *> phase2 RInt.ask)
  |> runWRintDay

let%test_unit "repmin: day naive example 2" =
  t |> repminDayAdv |> print_tree_by_levels

(* biased version of day functor *)
module BiasedDayFunctor =
functor
  (F : ApplicativeFunctorSig)
  (G : ApplicativeFunctorSig)
  ->
  struct
    type 'a f = 'a F.t
    type 'a g = 'a G.t
    type 'a t = Day' : ('b -> 'a) f * 'b g -> 'a t
  end

(* 5. multi phases *)

(*
 Pure no effect   
 Link add one more effectful phase 
*)
module PhaseFunctor =
functor
  (F : FunctorSig)
  ->
  struct
    type 'a f = 'a F.t

    (* each additional link = a combining function and a collection of values *)
    type 'a t =
      | Pure : 'a -> 'a t
      | Link : ('a * 'b -> 'c) * 'a f * 'b t -> 'c t
  end
(*
   module type PhaseFunctorSig = functor (F : ApplicativeFunctorSig) ->
     ApplicativeFunctorSig with type 'a t = 'a PhaseFunctor(F).t *)

module type PhaseFunctorSig = functor (F : ApplicativeFunctorSig) -> sig
  include module type of PhaseFunctor (F)
  include ApplicativeFunctorSig with type 'a t := 'a t
end

module PhaseApplicativeFunctorFree : functor (F : FunctorSig) ->
  ApplicativeFunctorSig with type 'a t = 'a PhaseFunctor(F).t =
functor
  (F : FunctorSig)
  ->
  struct
    include PhaseFunctor (F)

    let pure a = Pure a

    let ( <$> ) g = function
      | Pure v -> Pure (g v)
      | Link (f, xs, ys) ->
          let comp x = g (f x) in
          Link (comp, xs, ys)

    let rec cross : type a b. a t -> b t -> (a * b) t =
     fun phase1 phase2 ->
      match phase1 with
      | Pure x -> (fun y -> (x, y)) <$> phase2
      | Link (f, xs, ys) ->
          let comp (x, (y, z)) = (f (x, y), z) in
          Link (comp, xs, cross ys phase2)

    let ( <*> ) f a = (fun (f, a) -> f a) <$> cross f a
  end

module PhaseApplicativeFunctor : PhaseFunctorSig =
functor
  (F : ApplicativeFunctorSig)
  ->
  struct
    include PhaseFunctor (F)

    let pure a = Pure a

    let ( <$> ) f = function
      | Pure v -> Pure (f v)
      | Link (g, effect, phase) ->
          let apply_g_after_effect x = f (g x) in
          Link (apply_g_after_effect, effect, phase)

    let exch4 ((a, b), (c, d)) = ((a, c), (b, d))
    let fun_cross f g (x, y) = (f x, g y)

    (* polymorphic recursion function *)
    (* add explicit universal quantification to abstrac type a, b*)
    let rec cross : type a b. a t -> b t -> (a * b) t =
     fun left right ->
      match (left, right) with
      | Pure x, _ -> (fun z -> (x, z)) <$> right
      | _, Pure y -> (fun z -> (z, y)) <$> left
      | Link (f, xs, ys), Link (g, zs, ws) ->
          let module FHelper = Helper (F) in
          let combine_f_g x = x |> exch4 |> fun_cross f g in
          Link (combine_f_g, FHelper.cross xs zs, cross ys ws)

    let ( <*> ) f a = (fun (f, a) -> f a) <$> cross f a
  end

module PhaseHelper (F : ApplicativeFunctorSig) = struct
  include PhaseApplicativeFunctor (F)
  open Helper (F)

  let rec runPhase : type a. a t -> a f = function
    | Pure x -> F.pure x
    | Link (f, fa, b) ->
        let fb = runPhase b in
        let fab = cross fa fb in
        fmap f fab

  let now : 'a f -> 'a t = fun effect -> Link (unitr, effect, Pure ())
  let later : 'a t -> 'a t = fun phase -> Link (unitl, F.pure (), phase)

  let rec phase : int -> 'a f -> 'a t =
   fun level effect ->
    match level with 1 -> now effect | n -> later (phase (n - 1) effect)
end

module DayToPhase (F : ApplicativeFunctorSig) = struct
  module Day = DayWithSameEffect (F)
  module Phase = PhaseApplicativeFunctor (F)

  let inject : 'a Day.t -> 'a Phase.t = function
    | Day (f, phase_one, phase_two) ->
        Link (f, phase_one, Link (unitr, phase_two, Pure ()))
end

let%test_unit "multi-phase example" =
  let module Effect = DelayIdentityFunctor in
  let module Phase = PhaseApplicativeFunctor (Effect) in
  let module PhaseHelper = PhaseHelper (Effect) in
  let open PhaseHelper in
  let open Helper (Phase) in
  print_newline ();
  runPhase
    (phase 1 (fun () -> print_string "1")
    *> phase 2 (fun () -> print_string "2")
    *> phase 1 (fun () -> print_string "3")
    *> phase 3 (fun () -> print_string "6")
    *> phase 1 (fun () -> print_string "4")
    *> phase 2 (fun () -> print_string "5"))
    ()

(* sort tree problem: sort the tree into ascending order, then relabels the tree with the sorted list *)
(* express sort tree using multi phase
   phase 1. iterate the tree to get the list of element
   phase 2. sort the list of element in ascending order
   phase 3. traverse the tree again to relabel the tree according to the ascending order
*)

module type StateType = sig
  type t
end

module StateFunctor (S : StateType) :
  ApplicativeFunctorSig with type 'a t = S.t -> 'a * S.t = struct
  type 'a t = S.t -> 'a * S.t

  let pure : 'a -> 'a t = fun a s -> (a, s)

  let ( <$> ) f st s =
    let value, state = st s in
    (f value, state)

  let ( <*> ) f a s =
    let f, f_state = f s in
    let a, a_state = a f_state in
    (f a, a_state)
end

module StateFunctorHelper (S : StateType) = struct
  include StateFunctor (S)

  let ( let* ) : type a b. a t -> (a -> b t) -> b t =
   fun ma amb s ->
    let a, state = ma s in
    (amb a) state

  let get : S.t t = fun s -> (s, s)
  let put : S.t -> unit t = fun state _ -> ((), state)
  let modify (modifier : S.t -> S.t) : unit t = fun s -> ((), modifier s)
  let runState (m : 'a t) (s : S.t) = fst (m s)
end

(* mimicing second order polymorphism using type t *)
module ListStateFunctor (S : sig
  type t
end) =
struct
  type elem = S.t

  module ListState = struct
    type t = elem list
  end

  include StateFunctorHelper (ListState)

  let push : elem -> unit t = fun elem -> modify (fun s -> elem :: s)

  let pop =
    let* list_state = get in
    let* () = put (List.tl list_state) in
    pure (List.hd list_state)
end

let sort_tree (type a) (tree : a tree) : a tree =
  let module S = struct
    type t = a
  end in
  let module ListStateFunctor = ListStateFunctor (S) in
  let module PhaseHelper = PhaseHelper (ListStateFunctor) in
  let open DfsTreeTraversal (ListStateFunctor) in
  let open PhaseHelper in
  let open Helper (PhaseHelper) in
  let open ListStateFunctor in
  let sortTreeAux t =
    phase 1 (traverse push t)
    *> phase 2 (modify (List.sort compare))
    *> phase 3 (traverse (fun _ -> pop) t)
  in
  let state = runPhase (sortTreeAux tree) in
  runState state []

(*
3 - 1 - 1 
 \   \    
  \   5   
   \
    4 - 9
     \
      2
*)

(*
1 - 4 - 9 
 \   \    
  \   5   
   \
    1 - 3
     \
      2
*)

let t =
  node 3 [ node 1 [ node 1 []; node 5 [] ]; node 4 [ node 9 []; node 2 [] ] ]

let expected_t =
  node 1 [ node 1 [ node 2 []; node 3 [] ]; node 4 [ node 5 []; node 9 [] ] ]

let%test_unit "test sort tree" =
  let t = sort_tree t in
  print_newline ();
  print_tree_by_levels t;
  assert (t = expected_t)

let sort_tree_one_pass (type a) (tree : a tree) : a tree =
  let module S = struct
    type t = a
  end in
  let module ListStateFunctor = ListStateFunctor (S) in
  let module PhaseHelper = PhaseHelper (ListStateFunctor) in
  let open DfsTreeTraversal (PhaseHelper) in
  let open PhaseHelper in
  let open Helper (PhaseHelper) in
  let open ListStateFunctor in
  let sortTreeAux t =
    phase 2 (modify (List.sort compare))
    *> traverse (fun v -> phase 1 (push v) *> phase 3 pop) t
  in
  let state = runPhase (sortTreeAux tree) in
  runState state []

let%test_unit "test sort tree one pass" =
  let t = sort_tree_one_pass t in
  print_newline ();
  print_tree_by_levels t;
  assert (t = expected_t)

(* how to prove sort_tree_one_pass is faster than sort_tree *)
(*
I'm not too sure how to prove it
*)

(* breath first traversal *)

module BfsTreeTraversal : functor (F : ApplicativeFunctorSig) ->
  TraversalSig with type 'a f = 'a F.t and type 'a t = 'a tree =
functor
  (F : ApplicativeFunctorSig)
  ->
  struct
    type 'a t = 'a tree
    type 'a f = 'a F.t

    module PhaseFunctor = PhaseHelper (F)
    module ListTraversal = ListTraversal (PhaseFunctor)

    let rec bfsAux : type a b. (a -> b f) -> a t -> b t PhaseFunctor.t =
     fun f tree ->
      match tree with
      | Node (x, forest) ->
          let open PhaseFunctor in
          node
          <$> now (f x)
          <*> later (ListTraversal.traverse (bfsAux f) forest)

    let traverse f t =
      let open PhaseFunctor in
      t |> bfsAux f |> runPhase
  end

let%test_unit "bfs example" =
  print_newline ();
  let print_node v () = Printf.printf "%d\n" v in
  let module Bfs = BfsTreeTraversal (DelayIdentityFunctor) in
  let _ = (Bfs.traverse print_node t) () in
  ()

let bfl (type a b) (tree : a tree) (labels : b list) : b tree =
  let module ListStateFunctor = ListStateFunctor (struct
    type t = b
  end) in
  let module BfsTraversal = BfsTreeTraversal (ListStateFunctor) in
  ListStateFunctor.runState
    (BfsTraversal.traverse (fun _ -> ListStateFunctor.pop) tree)
    labels

let%test_unit "breath first relabelling" =
  let labels = [ 1; 1; 1; 1; 1; 1; 1 ] in
  let one_tree = bfl t labels in
  print_tree_by_levels t;
  print_tree_by_levels one_tree

let repmin_using_state_monad (tree : int tree) : int tree =
  let module StateFunctor = StateFunctorHelper (struct
    type t = int
  end) in
  let module PhaseFunctor = PhaseHelper (StateFunctor) in
  let module DfsTraversal = DfsTreeTraversal (PhaseFunctor) in
  let write x =
    let open StateFunctor in
    let* v = get in
    if x <= v then put x else pure ()
  in
  let fusion x =
    let open PhaseFunctor in
    let open Helper (PhaseFunctor) in
    phase 1 (write x) *> phase 2 StateFunctor.get
  in
  StateFunctor.runState
    (DfsTraversal.traverse fusion tree |> PhaseFunctor.runPhase)
    Int.max_int

(* repmin using state monad *)
let%test_unit "repmin using phase functor and state monad" =
  node 9 [ t ] |> repmin_using_state_monad |> print_tree_by_levels
