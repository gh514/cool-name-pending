
open Unix

let formula = Ast.Seq([Ast.Grid(3, 4)])

let out = Printf.printf

let close _ = out ")"

let nl _ = out "\n"

let translate_op op = 
  match op with 
    | Ast.Add -> out "(+" 
    | Ast.Sub -> out "(-"
    | Ast.Mul -> out "(*"
    | Ast.Div -> out "(/"
    | Ast.And -> out "(and "
    | Ast.Or -> out "(or "
    | Ast.Xor -> out "(xor "
    | Ast.Equal -> out "(= "
    | Ast.LT -> out "(< "
    | Ast.GT -> out "(> "
    | Ast.LTE -> out "(<= "
    | Ast.GTE -> out "(>= "
    | Ast.Unequal -> out "(distinct "
    | Ast.LeftImp -> out "(=> "
    | _ -> ()

let translate_uop uop = 
  match uop with
    | Ast.Neg -> out "(- "
    | Ast.Not -> out "(not "

let translate_quantifier q = 
  match q with
    | Ast.ForAll -> out "(forall "
    | Ast.Exists -> out "(exists "

let rec init_grid r c m =
  
  if c = 0 then 
    if r = 1 then ()
    else init_grid (r-1) m m
  else let outputs = out "(declare-const r%ic%i Int)" r c; nl () in 
    init_grid r (c-1) m; outputs

let rec translate_expr e = 
  (match e with 
    | Ast.Seq(_) -> ()
    | _ -> out " ");
  match e with 
    | Ast.Integer i -> out "%d" i
    | Ast.Boolean b -> out "%b" b
    | Ast.Op (e1, op, e2) -> 
      (match op with
      | Ast.RightImp -> translate_expr (Ast.Op(e2, Ast.LeftImp, e1))
      | Ast.BiImp -> translate_expr (Ast.Op(Ast.Op(e1, Ast.LeftImp, e2), Ast.And, Ast.Op(e2, Ast.LeftImp, e1)))
      | _ -> translate_op op; translate_expr e1; translate_expr e2; close ())
    | Ast.UnaryOp (uop, e) -> translate_uop uop; translate_expr e;
    | Ast.Seq (e::es) -> out "(assert"; translate_expr e; close (); nl (); translate_expr (Ast.Seq(es));
    | Ast.Quantifier (q, e1, e2, e3) -> translate_quantifier q; translate_expr e1; translate_expr e2; translate_expr e3;
    | Grid (r, c) -> init_grid r c c


    | _ -> ()



let _ = 
  out "(set-option :print-success false)\n";
  out "(set-logic AUFLIA)\n";
  translate_expr formula;
  

    
