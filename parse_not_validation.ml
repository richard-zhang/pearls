type ast = Repeat of int * ast | Str of string | Concat of ast * ast | End

let rec repeat n str = if n = 0 then "" else str ^ repeat (n - 1) str

let rec eval = function
  | Str str ->
      str
  | End ->
      ""
  | Concat (left, right) ->
      eval left ^ eval right
  | Repeat (num, ast) ->
      repeat num (eval ast)

let input_a = "3[ab]ac2[cd]"

(* 1. lookahead one token to decide the rule 2. construct left most derivation *)

type 'a parsec = char list -> ('a * char list) option

let ( let* ) (pa : 'a parsec) (pab : 'a -> 'b parsec) : 'b parsec =
 fun input ->
  let result = pa input in
  Option.bind result (fun (a, rest) -> (pab a) rest)

let ( let+ ) (pa : 'a parsec) (fab : 'a -> 'b) : 'b parsec =
 fun input ->
  let result = pa input in
  Option.map (fun (a, rest) -> (fab a, rest)) result

let return (type a) (x : a) : a parsec = fun input -> Some (x, input)

(* string refer to continuation *)
let parse_char c : char parsec = function
  | x :: xs when x = c ->
      Some (c, xs)
  | _ ->
      None

let rec split_until pred = function
  | x :: xs ->
      if pred x then
        let first, second = split_until pred xs in
        (x :: first, second)
      else ([], x :: xs)
  | [] ->
      ([], [])

let is_digit c =
  let code = Char.code c in
  code >= Char.code '0' && code <= Char.code '9'

let is_alpha c =
  let code = Char.code c in
  code >= Char.code 'a' && code <= Char.code 'z'

let parse_digit : int parsec =
 fun input ->
  let digit_string, rest = split_until is_digit input in
  let string = String.of_seq (List.to_seq digit_string) in
  let number = int_of_string_opt string in
  match number with Some number -> Some (number, rest) | None -> None

let parse_string : string parsec =
 fun input ->
  let raw_string, rest = split_until is_alpha input in
  match raw_string with
  | [] ->
      None
  | _ ->
      Some (String.of_seq (List.to_seq raw_string), rest)

(* abc3[abc]def *)
let rec parse_ast input =
  (* ll 1*)
  match input with
  | [] ->
      Some (End, [])
  | ']' :: _ ->
      Some (End, input)
  | x :: _ ->
      (let* ast = if is_digit x then parse_repeat else parse_str in
       let* rest = parse_ast in
       return (Concat (ast, rest)) )
        input

and parse_str input =
  (let+ string = parse_string in
   Str string )
    input

and parse_repeat input =
  (let* digit = parse_digit in
   let* _ = parse_char '[' in
   let* sub_expression = parse_ast in
   let* _ = parse_char ']' in
   return (Repeat (digit, sub_expression)) )
    input

let parse str = str |> String.to_seq |> List.of_seq |> parse_ast

let test_parse input_str expected _ =
  let parsed_result = parse input_str in
  match parsed_result with
  | Some (result, rest) ->
      OUnit2.assert_bool "must be empty" (rest = []) ;
      OUnit2.assert_equal result expected
  | None ->
      OUnit2.assert_failure "failed in parsing"

open OUnit2

let test_eval input_str expected =
  input_str
  >:: fun ctx ->
  let parsed_result = parse input_str in
  match parsed_result with
  | Some (result, rest) ->
      OUnit2.assert_bool "must be empty" (rest = []) ;
      OUnit2.assert_equal (eval result) expected
  | None ->
      OUnit2.assert_failure "failed in parsing"

let test_1 = test_eval "3[a]" "aaa"

let test_2 = test_eval "3[a]dc" "aaadc"

let test_3 = test_eval "3[ab]ac2[cd]" "abababaccdcd"

let test_4 = test_eval "3[a2[cd]ef]g" "acdcdefacdcdefacdcdefg"

let test_4 =
  test_eval "4[3[a2[d]]b]3[ab]2[2[f]]"
    "addaddaddbaddaddaddbaddaddaddbaddaddaddbabababffff"

let () =
  let suite = "suite" >::: [test_1; test_2; test_3; test_4] in
  run_test_tt_main suite
