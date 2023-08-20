open Lwt.Syntax

let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)

let pool = Lwt_domain.setup_pool 4

let parallel_compute x = Lwt_domain.detach pool (fun () -> fib x) ()

(* let action () =
  let* lwt_list =
    Lwt_stream.of_list [46; 46; 46]
    |> Lwt_stream.map_s parallel_compute
    |> Lwt_stream.to_list
  in
  Lwt_list.iter_p (Lwt_io.printf "%d\n") lwt_list *)

let action () =
  let* lwt_list = [46;46;46] |> Lwt_list.map_p parallel_compute in
  Lwt_list.iter_p (Lwt_io.printf "%d\n") lwt_list


let () = Lwt_main.run (action ())
