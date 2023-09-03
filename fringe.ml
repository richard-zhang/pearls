open Effect.Deep
open Effect

type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree

type 'a gen = unit -> 'a option

type 'a Effect.t += L : 'a -> unit Effect.t
let gen_tree (tree : 'a tree) : 'a gen = 
  let go = ref (failwith "GG") in
  let effect_handler = {
    effc = fun (type a) (eff : a t) -> match eff with
    | L element -> Some (fun (k: (a, _) continuation) -> go := fun () -> continue k (); Some element)
    | _ -> None
  }
  in !go

(*
1. 
*)

let same_fringe tree_one tree_two =
  let tree_one_generator = gen_tree tree_one in
  let tree_two_generator = gen_tree tree_two in
  let rec go () =
    match (tree_one_generator (), tree_two_generator ()) with
    | None, None ->
        true
    | Some a, Some b when a = b ->
        go ()
    | _, _ ->
        false
  in
  go ()
