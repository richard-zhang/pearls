type 'a tree = Leaf of 'a | Node of 'a tree list

let rec fold f g = function
  | Leaf x ->
      f x
  | Node ts ->
      g (List.map (fold f g) ts)

let rec unfold pred v h seed =
  if pred seed then Leaf (v seed)
  else Node (List.map (unfold pred v h) (h seed))

(*
  1. deforestation
    * Leaf case => f (v seed)
    * Node case =>
      * ts = (List.map (unfold pred v h) (h seed))
      * g (List.map (fold f g) ts)
      * g (List.map (fold f g) ((List.map (unfold pred v h) (h seed))))
      * g ((List.map (fold f g) . List.map (unfold pred v h)) (h seed))
      * g ((List.map (fold f g . unfold prev v h)) (h seed)) [fmap f. fmap g = fmap (f . g)]
      * g ((List.map (hylo f g p v h)) (h seed))
  * from the type signature, you can understand what do we mean by deforestation
  * there is no mention of Tree structure
*)

let hylo f g p h =
  let rec helper seed =
    if p seed then f seed else g (List.map helper (h seed))
  in
  helper

type 'a ltree = LLeaf of 'a | LNode of 'a * 'a ltree list

let label = function LLeaf a -> a | LNode (a, _) -> a

(* lleaf node rememebr the base case computation of (f x) *)
let lleaf f x = LLeaf (f x)

(* lnode remember the recursive case computation g on chilldren *)
let lnode g ts = LNode (g (List.map label ts), ts)

let fill f g = fold (lleaf f) (lnode g)

(*
  1. if unfold p id h is a genuine nexus   
    * A tree with shared nodes is called a nexus and a nexus arises with any recursion whose
      recursive subproblems overlap
    * A shared node is a node with more than one incoming edge
Note: with this definition, we have arrived at the central idea of the pearl. Suppose that the tree `unfold p id h` is a genuine nexus
and suppose we can apply fill f g to it wihout destroying sharing. Then hylo can be computed more efficiently than by the recursive method of (21.1) 
*)
let hylo_new f g p h x = x |> unfold p (fun x -> x) h |> fill f g |> label

let single = function [x] -> true | _ -> false

let mk_tree h = unfold single (fun x -> x) h

let hylo_list f g h seed = seed |> mk_tree h |> fold f g

(*
  The nexus has n(n + 1)/2 nodes for an input of length n  
*)
let isegs x =
  let init xs = xs |> List.rev |> List.tl |> List.rev in
  [init x; List.tl x]

let recover = function [a; b] -> List.hd a :: b | _ -> failwith "impossible"

(*
  1. binomial tree
  2. tree with iseg
  3. tree with minors
*)

(*
  * build the nexus layer by layer from bottom to top
  * layer is type that holds a layer of computation
  * in our case we have a layer of labelled tree

  * layer 
*)
type 'a layer = 'a list

let initialL (type a b) : (a list -> b) -> a list -> b ltree layer =
 fun f -> List.map (fun x -> [x] |> lleaf f)

let stepL (type b) : (b list -> b) -> b ltree layer -> b ltree layer =
 fun g x ->
  let rec group = function
    | [x] ->
        []
    | x :: y :: xs ->
        [x; y] :: group (y :: xs)
    | _ ->
        failwith "impossible"
  in
  x |> group |> List.map (lnode g)

let extractL (type b) : b ltree layer -> b ltree = List.hd

let singleL (type b) : b ltree layer -> bool = single
