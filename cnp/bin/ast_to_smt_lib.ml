

let formula = Ast.Seq([Ast.Grid(2, 2); Ast.Op(Ast.Boolean(true), Ast.LeftImp, Ast.UnaryOp(Ast.Not, Ast.Var("x")))])
let pformula = Past.Seq(0, [Past.Grid(0, 2, 2); 
  Past.Op(0, Past.Integer(0, 3), Past.Equal, Past.RC(0, Past.Integer(0, 1), Past.Integer(0, 1)));
  Past.Op(0, Past.Boolean(0, true), Past.BiImp, Past.UnaryOp(0, Past.Not, Past.Var(0, "x")))])

let out = Printf.printf

let close _ = out ")"

let nl _ = out "\n"

let translate_op op = 
  match op with 
    | Ast.Add -> out "(+" 
    | Ast.Sub -> out "(-"
    | Ast.Mul -> out "(*"
    | Ast.Div -> out "(/"
    | Ast.And -> out "(and"
    | Ast.Or -> out "(or"
    | Ast.Xor -> out "(xor"
    | Ast.Equal -> out "(="
    | Ast.LT -> out "(<"
    | Ast.GT -> out "(>"
    | Ast.LTE -> out "(<="
    | Ast.GTE -> out "(>="
    | Ast.Unequal -> out "(distinct"
    | Ast.LeftImp -> out "(=>"
    | _ -> ()

let translate_uop uop = 
  match uop with
    | Ast.Neg -> out "(-"
    | Ast.Not -> out "(not"
(*
let translate_quantifier q = 
  match q with
    | Ast.ForAll -> out "(forall "
    | Ast.Exists -> out "(exists "
    *)

let rec init_grid r c m =
  if c = 0 then 
    if r = 1 then ()
    else init_grid (r-1) m m
  else let outputs = out "(declare-const r%ic%i Int)" r c; nl (); in 
    init_grid r (c-1) m; outputs

    (*
let translate_dec t e =
  match t with
    | Ast.Cell -> ()
    | _ -> ()
    *)

let translate_var v =
  out "%s" v

let rec translate_expr e = 
  (match e with 
    | Ast.Seq(_) -> ()
    | _ -> out " ");
  match e with 
    | Ast.Integer i -> out "%d" i
    | Ast.Boolean b -> out "%b" b
    | Ast.Op (e1, op, e2) -> translate_op op; translate_expr e1; translate_expr e2; close ()
    | Ast.UnaryOp (uop, e) -> translate_uop uop; translate_expr e; close ()
    | Ast.Seq (e::es) -> out "(assert"; translate_expr e; close (); nl (); translate_expr (Ast.Seq(es));
(*    | Ast.Dec (t, e) -> translate_dec t e *)
    | Ast.Var (v) -> translate_var v
    | _ -> ()


let translate e =
  out "(set-option :print-success false)\n";
  out "(set-logic AUFLIA)\n";
  match e with
  | Ast.Seq(Ast.Grid (r, c) :: es) -> init_grid r c c; translate_expr (Ast.Seq(es))
  | _ -> out "Puzzle must have grid declaration"; nl ()


let _ = 
  let (e, _) = Past_to_ast.convert pformula []
    in translate e



  

    
