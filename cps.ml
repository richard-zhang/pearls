type ('a, 'r) cont = ('a -> 'r) -> 'r

let one : (int, 'r) cont = fun recevier -> recevier 1
let plus_one : int -> (int, 'r) cont = fun i receiver -> receiver (i + 1)

let multiple : int -> int -> (int, 'r) cont =
 fun a b receiver -> receiver (a * b)

let return (a : 'a) : ('a, 'r) cont = fun receiver -> receiver a

let ( >>= ) (type a b r) : (a, r) cont -> (a -> (b, r) cont) -> (b, r) cont =
 fun sender f receiver -> sender (fun a -> (f a) receiver)

let fmap (type a b r) : (a -> b) -> (a, r) cont -> (b, r) cont =
 fun f a_sender receiver -> a_sender (fun a -> receiver (f a))

let rec fact (i : int) : (int, 'r) cont =
  if i = 0 then return 1 else fmap (( * ) i) @@ fact (i - 1)
