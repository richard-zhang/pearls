open Effect
open Effect.Deep

type _ Effect.t += Xchg : int -> int t

type 'a status =
  | Complete of 'a
  | Suspended of {msg: int; cont: (int, 'a status) continuation}

let comp1 () = string_of_int (perform (Xchg 0) + perform (Xchg 1))

let step (f : unit -> 'a) () : 'a status =
  match_with f ()
    { retc= (fun v -> Complete v)
    ; exnc= raise
    ; effc=
        (fun (type a) (eff : a Effect.t) ->
          match eff with
          | Xchg msg ->
              Some (fun (k : (a, _) continuation) -> Suspended {msg; cont= k})
          | _ ->
              None ) }

let rec run_both a b =
  match (a (), b ()) with
  | Complete va, Complete vb ->
      (va, vb)
  | Suspended {msg= m1; cont= k1}, Suspended {msg= m2; cont= k2} ->
      run_both (fun () -> continue k1 m2) (fun () -> continue k2 m1)
  | _ ->
      failwith "Improper synchronization"

let comp1 () = perform (Xchg 0) + perform (Xchg 1)

let comp2 () = perform (Xchg 21) * perform (Xchg 21)

let value () =
  try_with comp1 ()
    { effc=
        (fun (type b) (eff : b t) ->
          match eff with
          | Xchg n ->
              Some (fun (k : (b, _) continuation) -> continue k (n * 2))
          | _ ->
              None ) }

type _ Effect.t += Fork : (unit -> unit) -> unit t | Yield : unit t

exception Improper_synchronization

let fork f = perform (Fork f)

let yield () = perform Yield

let xchg msg = perform (Xchg msg)

let run (main : unit -> unit) : unit =
  let exchanger = ref None in
  let run_q = Queue.create () in
  let enqueue k v =
    let task () = continue k v in
    Queue.push task run_q
  in
  let dequeue () =
    if Queue.is_empty run_q then
      match !exchanger with
      | None ->
          Printf.printf "empty\n" ; ()
      | Some (k, _) ->
          exchanger := None;
          discontinue k Improper_synchronization
    else
      let task = Queue.pop run_q in
      task ()
  in
  let rec spawn (f : unit -> unit) : unit =
    match_with f ()
      { retc= dequeue
      ; exnc=
          (fun e ->
            print_endline "hello";
            print_endline (Printexc.to_string e) ;
            dequeue () )
      ; effc=
          (fun (type a) (eff : a t) ->
            match eff with
            | Yield ->
                Some
                  (fun (k : (a, unit) continuation) -> enqueue k () ; dequeue ())
            | Fork f ->
                Some (fun (k : (a, unit) continuation) -> enqueue k () ; spawn f)
            | Xchg msg ->
                Some
                  (fun (k : (int, unit) continuation) ->
                    match !exchanger with
                    | None ->
                        exchanger := Some (k, msg) ;
                        Printf.printf "here\n" ;
                        dequeue ()
                    | Some (k', msg') ->
                        exchanger := None ;
                        enqueue k' msg ;
                        continue k msg' )
            | _ ->
                None ) }
  in
  spawn main

(* let _ =
     run (fun _ ->
         fork (fun _ ->
             Printf.printf "[t1] Sending 0\n" ;
             let v = xchg 0 in
             Printf.printf "[t1] received %d\n" v ) ;
         fork (fun _ ->
             Printf.printf "[t2] Sending 1\n" ;
             let v = xchg 1 in
             Printf.printf "[t2] received %d\n" v ) )

   let () =
     let a, b = run_both (step comp1) (step comp2) in
     print_int a ; print_newline () ; print_int b ; print_newline () *)

let _ =
  run (fun _ ->
      let v1 = xchg 1 in
      let v2 = xchg 2 in
      Printf.printf "received v1 %d\n" v1 ;
      Printf.printf "received v2 %d\n" v2 )
