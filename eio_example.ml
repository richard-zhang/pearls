(* ocamlfind ocamlc -package eio eio_main eio eio_stream.ml *)

(**
let _ =
  Eio_main.run @@ fun env ->
    let net = Eio.Stdenv.net env in
    Eio.Switch.run @@ fun sw ->
      Eio.Net.connect ~sw net (`Tcp (Eio.Net.Ipaddr.V4.loopback, 1234))
**)

(* Domain Manager *)
(*
let sum_to n =
  Eio.traceln "Starting CPU-intensive task..." ;
  let total = ref 0 in
  for i = 1 to n do
    total := !total + i
  done ;
  Eio.traceln "Finished" ;
  !total

let main ~domain_mgr =
  let test n =
    Eio.traceln "sum 1..%d = %d" n
      (Eio.Domain_manager.run domain_mgr (fun () -> sum_to n))
  in
  Eio.Fiber.both (fun () -> test 10000000) (fun () -> test 50000)
  (* Eio.Fiber.all [(fun () -> test 100000); (fun () -> test 50000)] *)

let () = Eio_main.run @@ fun env -> main ~domain_mgr:(Eio.Stdenv.domain_mgr env)
*)
(*
let () =
  Eio_main.run
  @@ fun _ ->
  let x : int ref = ref 0 in
  Eio.Fiber.both (fun () -> incr x) (fun () -> incr x) ;
  Eio.traceln "x = %d" !x
*)

(* let run fn =
  Eio_main.run
  @@ fun env ->
  let domain_mgr = Eio.Stdenv.domain_mgr env in
  fn (Eio.Domain_manager.run domain_mgr) *)

(*
let () = run @@ fun run_in_new_domain ->
    let x = ref 0 in
    Eio.Fiber.both (fun () -> run_in_new_domain (fun () -> incr x)) (fun () -> run_in_new_domain (fun () -> incr x));
    Eio.traceln "x = %d" !x
*)
(* let test run_in_new_domain =
  let x = Atomic.make 5 in
  let ready = Atomic.make false in
  let first_fiber () =
    run_in_new_domain (fun () ->
        Atomic.set x (Atomic.get x * 2) ;
        Atomic.set ready true )
  in
  let second_fiber () =
    run_in_new_domain (fun () ->
        while not (Atomic.get ready) do
          ()
        done ;
        Eio.traceln "x = %d" @@ Atomic.get x )
  in
  Eio.Fiber.both first_fiber second_fiber

let () = run test *)

let () = Eio_main.run @@ fun _ ->
  let stream = Eio.Stream.create 2 in
  Eio.Fiber.both
    (
      fun () ->
      for i = 1 to 5 do
        Eio.traceln "Adding %d.." i;
        Eio.Stream.add stream i
      done
    )
    (
      fun () ->
        for i = 1 to 5 do
          let x = Eio.Stream.take stream in
          Eio.traceln "Got %d" x;
        done
    )
