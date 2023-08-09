open Ppxlib
let _ = let loc = !Ast_helper.default_loc in [%expr string_of_int]
