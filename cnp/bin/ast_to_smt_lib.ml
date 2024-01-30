
(*
let formula = Ast.Seq([Ast.Grid(2, 2); Ast.Op(Ast.Boolean(true), Ast.LeftImp, Ast.UnaryOp(Ast.Not, Ast.Var("x")))])
let pformula = Past.Seq(0, [Past.Grid(0, 2, 2); 
  Past.Op(0, Past.Integer(0, 3), Past.Equal, Past.RC(0, Past.Integer(0, 1), Past.Integer(0, 1)));
  Past.Op(0, Past.Boolean(0, true), Past.BiImp, Past.UnaryOp(0, Past.Not, Past.Var(0, "x")))])
*)
let out = Printf.printf

let close _ = out ")"

let nl _ = out "\n"

let depth = ref (-1)

let rec indent n = match n with
  | 0 -> ()
  | _ -> out " "; indent (n-1)

let translate_op = function
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

let translate_uop = function
  | Ast.Neg -> out "(-"
  | Ast.Not -> out "(not"

let translate_mop = function
  | Ast.MultiAnd -> out "(and"
  | Ast.MultiOr -> out "(or"


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

let rec translate_list = function
  | e::es -> nl (); indent !depth; translate_expr e; translate_list es
  | [] -> ()

and translate_expr e = 
  (match e with 
    | Ast.Seq(_) -> ()
    | _ -> out " ");
  match e with 
    | Ast.Integer i -> out "%d" i
    | Ast.Boolean b -> out "%b" b
    | Ast.Op (e1, op, e2) -> translate_op op; translate_expr e1; translate_expr e2; close ()
    | Ast.UnaryOp (uop, e) -> translate_uop uop; translate_expr e; close ()
    | Ast.MultiOp (mop, e) -> depth := !depth+1; translate_mop mop; translate_list e; depth := !depth-1; close ()
    | Ast.Seq (e::es) -> out "(assert"; translate_expr e; close (); nl (); translate_expr (Ast.Seq(es));
(*    | Ast.Dec (t, e) -> translate_dec t e *)
    | Ast.Var (v) -> translate_var v
    | _ -> ()


let translate e =
  out "(set-option :print-success false)\n";
  out "(set-logic AUFLIA)\n";
  
  match e with
  | Ast.Seq(Ast.GridDec (r, c) :: es) -> init_grid r c c; translate_expr (Ast.Seq(es))
| _ -> out "Puzzle must have grid declaration"; nl ()

(*
let _ = 
  let (e, _) = Past_to_ast.convert pformula []
    in translate e

*)

  

    
