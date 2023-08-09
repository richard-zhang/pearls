type equation = Prim of string | Add of (string * string)

module StringMap = Map.Make (struct
  type t = string

  let compare = compare
end)

type eqns = equation StringMap.t

let uncons : string -> (char * string) option =
 fun input ->
  let len = String.length input in
  if len = 0 then None else Some (input.[0], String.sub input 1 (len - 1))

let parse eqns input =
  let tokens = String.split_on_char ' ' input in
  let symbol = List.nth tokens 0 in
  let equation =
    match List.length tokens with
    | 5 ->
        let a = List.nth tokens 2 in
        let b = List.nth tokens 4 in
        Add (a, b)
    | 3 ->
        let prim = List.nth tokens 2 in
        Prim prim
    | _ ->
        print_endline input ; failwith "GG wrong"
  in
  StringMap.add symbol equation eqns

let build_eqns = List.fold_left parse StringMap.empty

(* bit operation in OCaml *)
type char_set = int

let singleton (c : char) : char_set = 1 lsl (Char.code c - Char.code 'a')

let is_in (c : char) (set : char_set) = singleton c land set != 0

let rec sub_string symbol pattern =
  match (uncons symbol, uncons pattern) with
  | Some (x, xs), Some (y, ys) when x = y ->
      sub_string xs ys
  | Some (x, xs), Some (y, ys) when x != y ->
      sub_string xs pattern
  | _, _ ->
      pattern

let mk_look_up (equations : eqns) =
  (* using Imperative data structure like hash table *)
  let char_set_map = Hashtbl.create (StringMap.cardinal equations) in
  let rec lookup symbol =
    let result_opt = Hashtbl.find_opt char_set_map symbol in
    match result_opt with
    | Some value ->
        value
    | None ->
        let equation = StringMap.find symbol equations in
        let value =
          match equation with
          | Prim prim ->
              String.fold_left (fun set c -> set lor singleton c) 0 prim
          | Add (a, b) ->
                (let char_set_a = lookup a in
                 let char_set_b = lookup b in
                 char_set_a lor char_set_b )
        in
        Hashtbl.add char_set_map symbol value ;
        value
  in
  lookup

let solve raw_equations symbol pattern =
  let equations = raw_equations |> build_eqns in
  let lookup = mk_look_up equations in
  let rec helper pattern symbol =
    match uncons pattern with
    | Some (head, rest) ->
        let char_set = lookup symbol in
        (* check whether head exists in char_set_map *)
        if is_in head char_set then
          helper2 pattern (StringMap.find symbol equations)
        else pattern
    | None ->
        pattern
  and helper2 pattern eqn =
    match eqn with
    | Prim prim ->
        sub_string prim pattern
    | Add (a, b) ->
        helper (helper pattern a) b
  in
  0 == String.length @@ helper pattern symbol

let () =
  let test_cases = read_int () in
  for i = 1 to test_cases do
    let n = read_int () in
    let eqns = List.init n (fun _ -> read_line ()) in
    let symbol = read_line () in
    let pattern = read_line () in
    print_endline @@ if solve eqns symbol pattern then "YES" else "NO"
  done
