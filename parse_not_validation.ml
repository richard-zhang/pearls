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

let is_digit = function '0' .. '9' -> true | _ -> false

let is_alpha = function 'a' .. 'z' -> true | _ -> false

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

module ProperParser = struct
  open Angstrom

  let ast_parser =
    fix (fun ast_parser ->
        let* c = peek_char in
        match c with
        | None ->
            return End
        | Some ']' ->
            return End
        | Some a ->
            let+ ast =
              if is_digit a then
                let+ number = take_while1 is_digit >>| int_of_string <* char '['
                and+ sub_ast = ast_parser <* char ']' in
                Repeat (number, sub_ast)
              else
                let+ x = take_while1 is_alpha in
                Str x
            and+ rest = ast_parser in
            Concat (ast, rest) )

  let parse input =
    input
    |> parse_string ~consume:All ast_parser
    |> Result.to_option
    |> Option.map (fun x -> (x, []))
end

open OUnit2

let test_eval parse input_str expected =
  input_str
  >:: fun ctx ->
  let parsed_result = parse input_str in
  match parsed_result with
  | Some (result, rest) ->
      assert_bool "must be empty" (rest = []) ;
      assert_equal (eval result) expected
  | None ->
      assert_failure "failed in parsing"

let test_methods = [test_eval parse; test_eval ProperParser.parse]

let test_cases =
  [ ("3[a]", "aaa")
  ; ("3[a]dc", "aaadc")
  ; ("3[ab]ac2[cd]", "abababaccdcd")
  ; ("3[a2[cd]ef]g", "acdcdefacdcdefacdcdefg")
  ; ( "4[3[a2[d]]b]3[ab]2[2[f]]"
    , "addaddaddbaddaddaddbaddaddaddbaddaddaddbabababffff" ) ]

let () =
  let suite =
    "suite"
    >:::
    let open Base.List in
    test_methods
    >>= fun check ->
    test_cases >>= fun (input, output) -> return (check input output)
  in
  run_test_tt_main suite
