type 'a tree = Tip of 'a | Bin of 'a tree * 'a tree

let tip a = Tip a

let bin a b = Bin (a, b)

let mk_tree depth =
  let rec go x =
    if x = depth then tip depth
    else
      let t = go (x + 1) in
      bin t t
  in
  go 1

let rec flatten_slow = function
  | Tip x ->
      [x]
  | Bin (left, right) ->
      flatten_slow left @ flatten_slow right

let flatten_cps input =
  let rec go input cont =
    match input with
    | Tip x ->
        cont [x]
    | Bin (left, right) ->
        go left (fun l -> go right (fun r -> cont (l @ r)))
  in
  go input Fun.id

let flatten_fast input =
  let rec go input acc =
    match input with
    | Tip x ->
        x :: acc
    | Bin (left, right) ->
        go left (go right acc)
  in
  go input []

let example_input =
  bin
    (bin (tip 1) (bin (tip 2) (bin (bin (tip 3) (tip 4)) (tip 5))))
    (bin (tip 6) (tip 7))

(* let flatten_list = example_input |> flatten_fast *)

let () = Sys.argv.(1) |> int_of_string |> mk_tree |> flatten_fast |> ignore

(*
size    flatten_fast
1048575 39.1
2097151 44.9
4194303 89.3
8388607 174.6
16777215 349.5
*)