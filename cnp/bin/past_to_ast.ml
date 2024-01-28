

include Printf

exception Err of string

let gridr = ref 0
let gridc = ref 0

let translate_op = function
  | Past.Add -> Ast.Add
  | Past.Sub -> Ast.Sub
  | Past.Mul -> Ast.Mul
  | Past.Div -> Ast.Div
  | Past.And -> Ast.And
  | Past.Or -> Ast.Or
  | Past.Xor -> Ast.Xor
  | Past.Equal -> Ast.Equal
  | Past.LT -> Ast.LT
  | Past.GT -> Ast.GT
  | Past.LTE -> Ast.LTE
  | Past.GTE -> Ast.GTE
  | Past.Unequal -> Ast.Unequal
  | Past.LeftImp -> Ast.LeftImp
  | _ -> raise (Err "BiImp/RightImp error")

let translate_unary_op = function
  | Past.Neg -> Ast.Neg
  | Past.Not -> Ast.Not

let get_int = function
  | Past.Integer(_, n) -> n
  | _ -> raise (Err "Incorrect expression, Integer expected")

let get_bool = function
  | Past.Boolean(_, b) -> b
  | _ -> raise (Err "Incorrect expression, Boolean expected")

let get_var = function
  | Past.Var(_, v) -> v
  | _ -> raise (Err "Incorrect expression, Var expected")

let translate_rc r c vars = 
  (Ast.Var(sprintf "r%ic%i" (get_int r) (get_int c)), vars)

let bool_field m n = 
  let surrounding r c = 
    [(r-1, c-1); (r-1, c); (r-1, c+1);
     (r, c-1);             (r, c+1);
     (r+1, c-1); (r+1, c); (r+1, c+1)] in
  
  let create_vars r c =
    List.map (fun (rs, cs) -> Ast.Var(Printf.sprintf"r%ic%iTor%ic%i" r c rs cs))
      (List.filter (fun (r, c) -> r >= 1 && r <= m && c >= 1 && c<=n) (surrounding r c)) in

  let rec helper r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> helper (r-1) n
    | _, _ -> (create_vars r c) @ (helper r (c-1))
  in helper m n  

let translate_list l = 
  let rec helper = function
    | (Past.RC(_, r1, c1), Past.RC(_, r2, c2))::es -> 
      Ast.Op(Ast.Var(sprintf "r%ic%iTor%ic%i" (get_int r1) (get_int c1) (get_int r2) (get_int c2)), Ast.Equal, Ast.Boolean(true))
      :: helper es
    | [] -> []
    | _ -> raise (Err "Some list thing")
  in helper l

let rec translate_seq s vars =
  let var = ref [] in
  let rec helper seq v =
    match seq with
    | [e] -> let (ex, vx) = translate_expr e v in var := vx; [ex]
    | e::es -> let (ex, vx) = (translate_expr e v)
      in ex :: (helper es vx)
    | [] -> []
  in (Ast.Seq(helper s vars), !var)

and translate_term e1 op e2 vars =
  match translate_expr e1 vars with
  | (e1, _) -> match translate_expr e2 vars with
    | (e2, _) -> (Ast.Op(e1, translate_op op, e2), vars)

and translate_unary_term uop e vars =
  match translate_expr e vars with
  | (e, _) -> (Ast.UnaryOp(translate_unary_op uop, e), vars)

and translate_expr e vars = 
  match e with
  | Past.Integer(_, n) -> (Ast.Integer(n), vars)
  | Past.Boolean(_, b) -> (Ast.Boolean(b), vars)
  | Past.RC(_, r, c) -> translate_rc r c vars
  | Past.Var(_, v) -> (Ast.Var(v), (Ast.Var(v), None)::vars)
  | Past.Op(_, e1, op, e2) -> 
    (match op with
    | Past.RightImp -> translate_term e2 Past.LeftImp e1 vars
    | Past.BiImp -> translate_term e1 Past.Equal e2 vars
    | _ -> translate_term e1 op e2 vars)
  | Past.UnaryOp(_, uop, e) -> translate_unary_term uop e vars
  | Past.Seq(_, e) -> translate_seq e vars
  | Past.Grid(_, r, c) -> gridr := r; gridc := c; (Ast.Grid(r, c), vars)
(*  | Past.Dec(_, d, e) -> translate_dec d e vars *)
  | Past.List(_, es) -> (Ast.Seq(translate_list es), vars)
  | _ -> raise (Err "Not implemented yet")

  (*
and translate_dec d e vars =
  match d with
  | Past.Line -> let ex = translate_expr e vars in
    match ex with 
    | (v, vs) -> let field = bool_field !gridc !gridr in  
      (Ast.Group(v, field), (v, Some field) :: vs)

  *)

let convert = translate_expr