
exception Err of string

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
  | Ast.Abs -> out "(abs"

let rec init_grid r c m =
  if c = 0 then 
    if r = 1 then ()
    else init_grid (r-1) m m
  else let outputs = out "\n(declare-const r%ic%i Int)" r c; in 
    init_grid r (c-1) m; outputs
    
let rec translate_dec dt e =
  match dt with
  | Ast.Int -> out "(declare-const"; translate_expr e; out " Int)"
  | Ast.Bool -> out "(declare-const"; translate_expr e; out " Bool)"
  | Ast.Cell -> raise (Err "Type Ast.Cell should not be possible")
  | Ast.Region -> raise (Err "Type Ast.Region should not be possible")

and translate_list = function
  | e::es -> nl (); indent !depth; translate_expr e; translate_list es
  | [] -> ()

and translate_group = function
  | e::es -> translate_expr e; translate_group es
  | [] -> ()

and translate_bundle = function
  | e::es -> translate_expr e; translate_bundle es
  | [] -> ()

and translate_expr e = 
  (match e with
    | Ast.Bundle(_) -> ()
    | Ast.Dec(_, _) -> ()
    | _ ->  out " ");
  match e with 
    | Ast.Integer(i)-> out "%d" i
    | Ast.Boolean(b) -> out "%b" b
    | Ast.Op(e1, op, e2) -> translate_op op; translate_expr e1; translate_expr e2; close ()
    | Ast.UnaryOp(uop, e) -> translate_uop uop; translate_expr e; close ()
    | Ast.MultiOp(op, e) -> depth := !depth+1; (match e with 
      | [_] -> translate_list e; depth := !depth-1
      | _ -> translate_op op; translate_list e; depth := !depth-1; close ())
    | Ast.Var(v) -> out "%s" v
    | Ast.Dec(dt, e) -> translate_dec dt e
    | Ast.Bundle(es) -> translate_bundle es
    | Ast.ITE(c, e1, e2) -> out "(ite"; translate_expr c; translate_expr e1; translate_expr e2; close ()
    | _ -> ()

let translate p =
  out "(set-option :print-success false)\n";
  out "(set-logic AUFLIA)\n";

  match p with
    | ((r, c), xs) -> init_grid r c c;
      let rec loop = function
        | e::es -> (match e with
          | Ast.Dec(_, _) -> nl(); translate_expr e; loop es
          | Ast.Bundle(_) -> nl(); translate_expr e; loop es
          | _ ->  out "\n(assert"; translate_expr e; close(); loop es)
        | [] -> ()
      in loop xs; out "\n\n(check-sat)\n(get-model)\n"
    | _ -> out "Puzzle must begin with grid declaration"; nl ();
