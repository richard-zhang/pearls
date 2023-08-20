type var = string

type term =
  | Var of var
  | Fix of (var * var list * term) list * term
  | Appl of term * term list
  | Record of term list
  | Select of term * int

type cps_var = CLamVar of var | CGenVar of int

type cps_term =
  | CFix of (cps_var * cps_var list * cps_term) list * cps_term
  | CAppl of cps_var * cps_var list
  | CRecord of cps_var list * binder
  | CSelect of cps_var * int * binder
  | CHalt of cps_var

and binder = cps_var * cps_term

let gensym i =
  let x = CGenVar !i in
  i := !i + 1 ;
  x

let rec convert gen finish = function
  | Var x ->
      finish (CLamVar x)
  | Fix (defs, m) ->
      CFix (List.map (convert_def gen) defs, convert gen finish m)
  | Appl (f, args) ->
      let ret_k = gensym gen in
      let ret_k_x = gensym gen in
      CFix
        ( [(ret_k, [ret_k_x], finish ret_k_x)]
        , f
          |> convert gen (fun f_cps ->
                 args
                 |> convert_list gen (fun args_cps ->
                        CAppl (f_cps, args_cps @ [ret_k]) ) ) )
  | Record fields ->
      fields
      |> convert_list gen (fun fields_cps ->
             let x = gensym gen in
             CRecord (fields_cps, (x, finish x)) )
  | Select (m, i) ->
      m
      |> convert gen (fun m_cps ->
             let x = gensym gen in
             CSelect (m_cps, i, (x, finish x)) )

and convert_list gen finish =
  let rec go acc = function
    | [] ->
        finish (List.rev acc)
    | x :: xs ->
        x |> convert gen (fun x_cps -> go (x_cps :: acc) xs)
  in
  go []

and convert_def gen (f, params, m) =
  let k = gensym gen in
  ( CLamVar f
  , List.map (fun x -> CLamVar x) params @ [k]
  , m |> convert gen (fun m_cps -> CAppl (k, [m_cps])) )
