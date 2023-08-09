let rec read_lines lines =
  try
    let line = input_line stdin in
    read_lines (line :: lines)
  with
  | End_of_file -> lines

let parse_line line =
    let (a :: [b]) = String.split_on_char ' ' line in
    (int_of_string a, int_of_string b)
  

let () =
  let (num_tree, num_person) = input_line stdin |> parse_line in
  Printf.printf "(num_tree=%d, num_person=%d)\n" num_tree num_person;
  let lines = read_lines [] in
  List.map parse_line lines
  |> List.iter (fun (a,b) -> Printf.printf "(%d,%d)\n" a b)