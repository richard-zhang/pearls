module StringSet = Set.Make (struct
  type t = string

  let compare = String.compare
end)

(* brute force recursion *)
let sol (dict : StringSet.t) (raw_sentence : string) : string =
  let is_string word = StringSet.mem word dict in
  let rec go str_seq to_check ans =
    match str_seq () with
    | Seq.Cons (x, xs) ->
        let search_word = to_check ^ Char.escaped x in
        if is_string search_word then
          let result = go xs "" (search_word :: ans) in
          if Option.is_some result then result else go xs search_word ans
        else go xs search_word ans
    | Seq.Nil ->
        if String.length to_check = 0 then Some ans else None
  in
  let result = go (String.to_seq raw_sentence) "" [] in
  match result with
  | None ->
      failwith "cannot be segmented"
  | Some words ->
      words |> List.rev |> String.concat " "

let test_case_one =
  let test_one () =
    let test_string = "BOTHEARTHANDSATURNSPIN" in
    let test_dict =
      StringSet.of_list ["BOTH"; "EARTH"; "AND"; "SATURN"; "SPIN"]
    in
    Alcotest.(check string)
      "boilerplate" "BOTH EARTH AND SATURN SPIN"
      (sol test_dict test_string)
  in
  Alcotest.test_case "boilerplate" `Quick test_one

let test_case_two =
  let test_one () =
    let test_string = "ocamlisfun" in
    let test_dict = StringSet.of_list ["oc"; "ocaml"; "is"; "fun"] in
    Alcotest.(check string)
      "boilerplate" "ocaml is fun"
      (sol test_dict test_string)
  in
  Alcotest.test_case "boilerplate" `Quick test_one

let () =
  Alcotest.run "boilerplate" [("boilerplate", [test_case_one; test_case_two])]
