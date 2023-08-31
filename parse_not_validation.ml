type ast = Repeat of int * ast | Str of string | Concat of ast * ast | End

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
      parse_end input
  | x :: _ ->
      (let* ast = if is_digit x then parse_repeat else parse_str in
       let* rest = parse_ast in
       return (Concat (ast, rest)) )
        input

and parse_end input = (return End) input

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
