

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

let rec substitute var_new var_old expr =
  match expr with
  | Past.Var(l, v) -> if v = var_old then Past.Var(l, var_new) else Past.Var(l, v)
  | Past.Op(l, e1, op, e2) -> Past.Op(l, substitute var_new var_old e1, op, substitute var_new var_old e2)
  | Past.UnaryOp(l, uop, e) -> Past.UnaryOp(l, uop, substitute var_new var_old e)
  | Past.Seq(l, es) -> (match es with
    | h::tl -> (match substitute var_new var_old (Past.Seq(l, tl)) with
      | Seq(_, seq) -> Past.Seq(l, (substitute var_new var_old h) :: seq))
    | [] -> Past.Seq(l, []))
  | Past.Quantifier(l, q, d, g, e) -> Past.Quantifier(l, q, d, g, substitute var_new var_old e)
  | e -> e

let apply_forall vars v_old c =
  List.map (fun v -> substitute v v_old c) vars

let cell_grid l = 
  let m = !gridr in
  let n = !gridc in
  let rec loop r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> loop (r-1) n
    | _, _ -> Past.Var(l, sprintf "r%ic%i" r c) :: (loop r (c-1))
  in List.rev (loop m n)

let bool_grid l reg =
  let m = !gridr in
  let n = !gridc in
  let rec loop r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> loop (r-1) n
    | _, _ -> Past.Var(l, sprintf "r%ic%i_in_%s" r c reg) :: (loop r (c-1))
  in List.rev (loop m n)

let bool_field = 
  let m = !gridr in
  let n = !gridc in
  let surrounding r c = 
    [(r+1, c+1); (r+1, c); (r+1, c-1);
     (r, c+1);             (r, c-1);
     (r-1, c+1); (r-1, c); (r-1, c-1)] in
  
  let create_vars r c =
    List.map (fun (rs, cs) -> Ast.Var(sprintf"r%ic%iTor%ic%i" r c rs cs))
      (List.filter (fun (r, c) -> r >= 1 && r <= m && c >= 1 && c<=n) (surrounding r c)) in

  let rec loop r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> loop (r-1) n
    | _, _ -> (create_vars r c) @ (loop r (c-1))
  in List.rev (loop m n)

(*
let translate_list l = 
  let rec loop = function
    | (Past.RC(_, r1, c1), Past.RC(_, r2, c2))::es -> 
      Ast.Var(sprintf "r%ic%iTor%ic%i" (get_int r1) (get_int c1) (get_int r2) (get_int c2))
      :: loop es
    | [] -> []
    | _ -> raise (Err "Some list thing")
  in loop l
*)



let rec translate_seq s vars =
  let var = ref [] in
  let rec loop seq v =
    match seq with
    | [e] -> let (ex, vx) = translate_expr e v in var := vx; [ex]
    | e::es -> let (ex, vx) = (translate_expr e v) in ex :: (loop es vx)
    | [] -> []
  in (Ast.Seq(loop s vars), !var)

and translate_term e1 op e2 vars =
  match translate_expr e1 vars with
  | (e1, _) -> match translate_expr e2 vars with
    | (e2, _) -> (Ast.Op(e1, translate_op op, e2), vars)

and translate_unary_term uop e vars =
  match translate_expr e vars with
  | (e, _) -> (Ast.UnaryOp(translate_unary_op uop, e), vars)


and translate_dec d e vars = 
  match d with
  | Past.Cell -> raise (Err "Cannot declare Cell")
  | Past.Region -> let (v, nvars) = translate_expr e vars in
    let field = bool_grid (get_var v)
    (Ast.Group(v))

  (*
  | Past.Line -> let ex = translate_expr e vars in
    match ex with 
    | (v, vs) -> let field = bool_field !gridc !gridr in  
      (Ast.Group(v, field), (v, Some field) :: vs)
    | _ -> raise (Err "Line declaration error")
*)
    
and translate_quantifier l q d g c vars=
  let op = ref Ast.MultiAnd in
  (match q with
    | Past.ForAll -> ()
    | Past.Exists -> op := Ast.MultiOr);
  match d with
    | Past.Dec(_, Past.Cell, var) -> 
        (Ast.MultiOp(!op, (List.map (fun v -> let (expr, _) = 
        translate_expr (substitute (get_var v) (get_var var) c) vars in expr) (cell_grid l))), vars)
        

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
  | Past.GridDec(_, r, c) -> gridr := r; gridc := c; (Ast.GridDec(r, c), vars)
  | Past.Dec(_, d, v) -> translate_dec d v vars   
  | Past.List(_, es) -> (Ast.Seq(translate_list es), vars)
  | Past.Quantifier(l, q, d, g, c) -> translate_quantifier l q d g c vars


let rec flatten = function
  | Ast.Seq(l) -> 
    let rec loop = function
      | x::xs -> (flatten x) @ (loop xs)
      | [] -> []
    in loop l
  | e -> [e]

let convert e = let (expr, v) = translate_expr e [] in 
  (Ast.Seq(flatten expr), v)

