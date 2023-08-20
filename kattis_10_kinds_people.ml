let solve (start_p : int * int) (end_p : int * int) (map : int array array) :
    string =
  ""

let () =
  let row = read_int () in
  print_int row;
  let col = read_int () in
  let map = Array.make_matrix row col 0 in
  for i = 1 to row do
    let row_content = read_line () in
    String.iteri (fun j c -> map.(i - 1).(j) <- int_of_char c) row_content
  done ;
  let number_of_query = read_int () in
  for i = 1 to number_of_query do
    let start_r = read_int () in
    let start_c = read_int () in
    let end_r = read_int () in
    let end_c = read_int () in
    print_endline @@ solve (start_r, start_c) (end_r, end_c) map
  done
