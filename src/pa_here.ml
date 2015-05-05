open Camlp4.PreCast

let dirname = ref None
let () =
  Camlp4.Options.add "-pa-here-dirname" (Arg.String (fun s -> dirname := Some s))
  "Should be the name of the current directory (when camlp4 is called) relative to the \
   root of the project (use to give better positions)."

let ast_of_loc _loc : Ast.expr =
  let pos =
    (* when we are given a dirname, we compute the path relative to the root
       when we are not given a dirname, we use the full path *)
    Loc.start_pos (
      match !dirname with
      | None -> Loc.make_absolute _loc
      | Some _ -> _loc
    ) in
  let lnum = string_of_int pos.Lexing.pos_lnum in
  let cnum = string_of_int pos.Lexing.pos_cnum in
  let bol = string_of_int pos.Lexing.pos_bol in
  let fname = pos.Lexing.pos_fname in
  let fname =
    match Filename.is_relative fname, !dirname with
    | true, Some dirname -> Filename.concat dirname fname
    | true, None
    | false, _ -> fname in
  <:expr<{
    Lexing.pos_fname = $str:fname$;
    pos_lnum = $int:lnum$;
    pos_cnum = $int:cnum$;
    pos_bol = $int:bol$
  } >>

EXTEND Gram
  Syntax.expr: LEVEL "simple" [
    [ "_here_" -> ast_of_loc _loc ]
  ];
END
