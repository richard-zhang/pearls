let rec read_lines lines =
  try
    let line = input_line stdin in
    read_lines (line :: lines)
  with
  | End_of_file -> lines

[@@@warning "-8"]
let parse_line line =
    let (a :: [b]) = String.split_on_char ' ' line in
    (int_of_string a, int_of_string b)

module ListIntSet = Set.Make (struct
  type t = int list

  let compare = compare
end)

let solve num_person num_tree views =
  let array = Array.make num_person [] in
  List.iter (fun (p, t) -> array.(p - 1) <- t :: array.(p - 1)) views ;
  let array = Array.map (List.sort_uniq compare) array in
  Array.fold_left (fun acc b -> ListIntSet.add b acc) ListIntSet.empty array
  |> ListIntSet.cardinal

let () =
  let (num_person, num_tree) = input_line stdin |> parse_line in
  let views = read_lines [] |> List.map parse_line in
  Printf.printf "%d\n" @@ solve num_person num_tree views
