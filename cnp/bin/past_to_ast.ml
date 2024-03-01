
include Printf

exception Err of string

let gridr = ref 0
let gridc = ref 0
let regions = ref false

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

let get_bool = function
  | Past.Boolean(_, b) -> b

let get_var = function
  | Past.Var(_, v) -> v

let get_var_ast = function
  | Ast.Var(v) -> v

let rec substitute var_new var_old expr =
  match expr with
  | Past.Var(l, v) -> if v = var_old then Past.Var(l, var_new) else Past.Var(l, v)
  | Past.Op(l, e1, op, e2) -> Past.Op(l, substitute var_new var_old e1, op, substitute var_new var_old e2)
  | Past.UnaryOp(l, uop, e) -> Past.UnaryOp(l, uop, substitute var_new var_old e)
  | Past.RegionOp(l, e1, rop, e2) -> Past.RegionOp(l, substitute var_new var_old e1, rop, substitute var_new var_old e2)
  | Past.Quantifier(l, q, d, g, e) -> Past.Quantifier(l, q, d, g, substitute var_new var_old e)
  | Past.Utils(l, e, u) -> Past.Utils(l, substitute var_new var_old e, u)
  | e -> e

let rec ast_to_past location l =
  match l with
  | Ast.Var(x)::ls -> Past.Var(location, x)::(ast_to_past location ls)
  | [] -> []

let make_var_pair r1 c1 r2 c2 = Ast.Var(sprintf "r%ic%iTor%ic%i" r1 c1 r2 c2)

let cells () =
  let m = !gridr in
  let n = !gridc in
  let rec loop r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> loop (r-1) n
    | _, _ -> (r, c) :: loop r (c-1)
  in loop m n

let cell_grid l = 
  let m = !gridr in
  let n = !gridc in
  let rec loop r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> loop (r-1) n
    | _, _ -> Past.Var(l, sprintf "r%ic%i" r c) :: (loop r (c-1))
  in loop m n

let bool_grid reg =
  let m = !gridr in
  let n = !gridc in
  let rec loop r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> loop (r-1) n
    | _, _ -> Ast.Var(sprintf "r%ic%i_in_%s" r c reg) :: (loop r (c-1))
  in loop m n

let int_grid str = 
  let m = !gridr in
  let n = !gridc in
  let rec loop r c =
    match r, c with
    | 0, _ -> []
    | _, 0 -> loop (r-1) n
    | _, _ -> Ast.Var(sprintf "r%ic%i_%s" r c str) :: (loop r (c-1))
  in loop m n

let rec unzip = function
  | (a, b)::ls -> a::b::(unzip ls)
  | [] -> []

let adj r c = 
  let m = !gridr in
  let n = !gridc in
  List.map (fun (rs, cs) -> ((r, c), (rs, cs)))
  (List.filter (fun (r, c) -> r >= 1 && r <= m && c >= 1 && c <= n) [(r+1, c); (r, c+1); (r, c-1); (r-1, c)])

let create_vars () =
  let m = !gridr in
  let n = !gridc in
  
  let rec grid_pairs r c offset =
      match r, c with
      | 0, _ -> []
      | -1, _ -> []
      | _, 0 -> grid_pairs (r-1) (n-offset) (1-offset)
      | _, -1 -> grid_pairs (r-1) (n-offset) (1-offset)
      | _, _ -> (adj r c) @ (grid_pairs r (c-2) offset)

  in grid_pairs m n 1

let get_vars pairs = List.map (fun ((r1, c1), (r2, c2)) -> 
  (make_var_pair r1 c1 r2 c2, make_var_pair r2 c2 r1 c1)) pairs

let create_line l = let Ast.Var(nl) = l in List.map (fun (Ast.Var(v1), Ast.Var(v2)) 
  -> (Ast.Var(sprintf "%s_%s" nl v1), Ast.Var(sprintf "%s_%s" nl v2))) (get_vars (create_vars ()))

let direction_constraints f = 
  let rec loop = function
    | (v1, v2)::ls -> Ast.UnaryOp(Ast.Not, Ast.Op(v1, Ast.And, v2)) :: (loop ls)
    | [] -> []
  in loop f

let get_parent_constraints () = 
  let m = !gridr in
  let n = !gridc in

  let rec parent_constraints r c =
    let adj_cells = adj r c in
    List.map (fun ((_, _), (rx, cx)) -> 
      Ast.Op(make_var_pair rx cx r c, Ast.LeftImp, Ast.UnaryOp(Ast.Not, 
        Ast.MultiOp(Ast.Or, 
        (let rec loop = function
          | ((_, _), (ry, cy))::ls -> (make_var_pair ry cy r c) :: (loop ls)
          | [] -> []
        in loop (List.filter (fun ((_, _), (rz, cz)) -> not ((rx, cx) = (rz, cz))) adj_cells)
        ))
    )))
    adj_cells

  in let rec parent_loop r c = 
    match (r, c) with
    | 0, _ -> []
    | _, 0 -> parent_loop (r-1) n
    | _, _ -> (parent_constraints r c) @ parent_loop r (c-1)

  in parent_loop m n

let get_children_constraints () =
  let m = !gridr in
  let n = !gridc in

  let children_constraints r c =
    let adj_cells = adj r c in
    Ast.Op(Ast.Var(sprintf "r%ic%i_count" r c), Ast.Equal,
      Ast.MultiOp(Ast.Add, Ast.Integer(1) ::
        (let rec loop = function
          | ((_, _), (ry, cy))::ls -> Ast.ITE(
            make_var_pair r c ry cy, Ast.Var(sprintf "r%ic%i_count" ry cy), Ast.Integer(0)) :: loop ls
          | [] -> []
        in loop adj_cells
        )))

  in let rec child_loop r c =
    match (r, c) with
    | 0, _ -> []
    | _, 0 -> child_loop (r-1) n
    | _, _ -> (children_constraints r c) :: child_loop r (c-1)

  in child_loop m n

let size_constraints f =
  let rec loop = function
    | ((r1, c1), (r2, c2)) :: ls -> Ast.Op(Ast.Op(make_var_pair r1 c1 r2 c2, Ast.Or, make_var_pair r2 c2 r1 c1), Ast.LeftImp, 
      Ast.Op(Ast.Var(sprintf "r%ic%i_size" r1 c1), Ast.Equal, Ast.Var(sprintf "r%ic%i_size" r2 c2))) :: (loop ls)
    | [] -> []
  in loop f

let origin_constraints f = 
  let rec loop = function
    | ((r1, c1), (r2, c2)) :: ls -> Ast.Op(Ast.Op(make_var_pair r1 c1 r2 c2, Ast.Or, make_var_pair r2 c2 r1 c1), Ast.LeftImp, 
      Ast.Op(Ast.Var(sprintf "r%ic%i_root" r1 c1), Ast.Equal, Ast.Var(sprintf "r%ic%i_root" r2 c2)))::(loop ls)
    | [] -> []
  in loop f

let get_root_constraints () =
  let m = !gridr in
  let n = !gridc in

  let root_constraints r c =
    Ast.Op(Ast.UnaryOp(Ast.Not, Ast.MultiOp(Ast.Or, 
      List.map (fun ((_, _), (rx, cx)) -> make_var_pair rx cx r c) (adj r c))), Ast.LeftImp,
      Ast.MultiOp(Ast.And, [Ast.Op(Ast.Var(sprintf "r%ic%i_size" r c), Ast.Equal, Ast.Var(sprintf "r%ic%i_count" r c));
        Ast.Op(Ast.Var(sprintf "r%ic%i_root" r c), Ast.Equal, Ast.Integer((r-1)*n + c));
        Ast.Op(Ast.Var(sprintf "r%ic%i_num" r c), Ast.Equal, Ast.Var(sprintf "r%ic%i_sum" r c))])
      )

  in let rec root_loop r c =
    match (r, c) with
    | 0, _ -> []
    | _, 0 -> root_loop (r-1) n
    | _, _ -> (root_constraints r c) :: root_loop r (c-1)

  in root_loop m n

let sum_constraints f = 
  let rec loop = function
    | ((r1, c1), (r2, c2)) :: ls -> Ast.Op(Ast.Op(make_var_pair r1 c1 r2 c2, Ast.Or, make_var_pair r2 c2 r1 c1), Ast.LeftImp, 
      Ast.Op(Ast.Var(sprintf "r%ic%i_sum" r1 c1), Ast.Equal, Ast.Var(sprintf "r%ic%i_sum" r2 c2)))::(loop ls)
    | [] -> []
  in loop f

let get_total_constraints () =
  let m = !gridr in
  let n = !gridc in
  
  let total_constraints r c =
    let adj_cells = adj r c in
    Ast.Op(Ast.Var(sprintf "r%ic%i_num" r c), Ast.Equal,
      Ast.MultiOp(Ast.Add, Ast.Var(sprintf "r%ic%i" r c) ::
        (let rec loop = function
          | ((_, _), (ry, cy))::ls -> Ast.ITE(
            make_var_pair r c ry cy, Ast.Var(sprintf "r%ic%i_num" ry cy), Ast.Integer(0)) :: loop ls
          | [] -> []
        in loop adj_cells
        )))
  
  in let rec total_loop r c =
    match (r, c) with
    | 0, _ -> []
    | _, 0 -> total_loop (r-1) n
    | _, _ -> (total_constraints r c) :: total_loop r (c-1)
  
  in total_loop m n

let init_regions _ =
  let grid = create_vars () in
  let field = List.map (fun v -> Ast.Dec(Ast.Bool, v)) (unzip (get_vars grid)) in
  let constr_field = direction_constraints (get_vars grid) in
  let size_grid = List.map (fun v -> Ast.Dec(Ast.Int, v)) (int_grid "size") in
  let count_grid = List.map (fun v -> Ast.Dec(Ast.Int, v)) (int_grid "count") in
  let root_grid = List.map (fun v -> Ast.Dec(Ast.Int, v)) (int_grid "root") in
  let num_grid = List.map (fun v -> Ast.Dec(Ast.Int, v)) (int_grid "num") in
  let sum_grid = List.map (fun v -> Ast.Dec(Ast.Int, v)) (int_grid "sum") in
  let parent_constr = get_parent_constraints () in
  let children_constr = get_children_constraints () in
  let size_constr = size_constraints grid in
  let root_constr = get_root_constraints () in
  let origin_constr = origin_constraints grid in
  let sum_constr = sum_constraints grid in
  let total_constr = get_total_constraints () in
    Ast.Bundle(field @ size_grid @ count_grid @ root_grid @ num_grid @ sum_grid @ constr_field @ parent_constr
     @ children_constr @ size_constr @ root_constr @ origin_constr @ sum_constr @ total_constr)

let translate_list xs = 
  let rec loop = function
    | (Past.RC(l, r, c))::es -> Past.Var(l, sprintf "r%ic%i" (get_int r) (get_int c)) :: loop es
    | [] -> []
    | _ -> raise (Err "Some list thing")
  in loop xs

let rec match_var v = function
  | x::xs -> if Str.string_match (Str.regexp (get_var x)) v 0 then true else match_var v xs
  | [] -> false
  
let rec translate_term e1 op e2 vars =
  let (expr1, vars1) = translate_expr e1 vars in
  let (expr2, vars2) = translate_expr e2 vars1 in
  (Ast.Op(expr1, translate_op op, expr2), vars2)

and translate_unary_term uop e vars =
  let (expr, _) = translate_expr e vars in
  (Ast.UnaryOp(translate_unary_op uop, expr), vars)

and translate_dec d v e vars = 
  let helper v vars =
    let (nv, _) = translate_expr v vars in
    match d with
    | Past.Int -> (Ast.Dec(Ast.Int, nv), (Ast.Int, nv, None)::vars)
    | Past.Cell -> (Ast.Dead, (Ast.Cell, nv, None)::vars)
    | Past.Region -> let init = if !regions then Ast.Dead else (regions := true; init_regions ()) in
      (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> 
        translate_expr l vars 


        let cells = List.map (fun (Past.RC(_, r, c)) -> (get_int r, get_int c)) l in
        let (r_root, c_root) = List.hd cells in (Ast.Bundle(init::[Ast.MultiOp(Ast.And, List.map (fun (r, c) -> 
          Ast.Op(Ast.Var(sprintf "R%iC%i_root" r c), Ast.Equal, Ast.Integer(!gridc*(r_root-1)+c_root))) cells)])
        , (Ast.Region, nv, Some cells)::vars)
      | None -> (init, (Ast.Region, nv, None)::vars))
    | Past.Line -> (Ast.Bundle(unzip (create_line nv)), (Ast.Line, nv, None)::vars)
    | _ -> raise (Err "Unimplemented datatype")

  in let rec loop vars = function
    | v::vs -> let (ne, nvars) = helper v vars in (ne, nvars)::(loop nvars vs)
    | [v] -> [helper v vars]
    | [] -> []

  in match v with 
  | Past.List(_, l) -> let nl = loop vars l in
    let (es, vs) = List.split nl in

    (Ast.Bundle(es), List.nth vs ((List.length vs)-1))

and translate_group l g = match g with
  | Past.Grid -> cell_grid l
  | Past.Instance(Past.List(_, es)) -> translate_list es
    
and translate_quantifier l q d g c vars =
  match q with
  | Past.NForAll -> translate_expr (Past.UnaryOp(l, Past.Not, Past.Quantifier(l, q, d, g, c))) vars
  | Past.NExists -> translate_expr (Past.UnaryOp(l, Past.Not, Past.Quantifier(l, q, d, g, c))) vars
  | _ -> let op = match q with
  | Past.ForAll -> Ast.And
  | Past.Exists -> Ast.Or
  in match d with
    | Past.Dec(_, Past.Cell, Past.List(_, var), value) -> 
      let split = function
      | [vx] -> Ast.MultiOp(op, (List.map (fun v -> let (expr, _) = 
        translate_expr (substitute (get_var v) (get_var vx) c) vars in expr) (translate_group l g)))
      | vx::vs -> let (nv, _) = translate_quantifier l q (Past.Dec(l, Past.Cell, Past.List(l, [vx]), value)) g
        (Past.Quantifier(l, q, (Past.Dec(l, Past.Cell, Past.List(l, vs), value)), g, c)) vars
        in nv
      in (split var, vars)
    | Past.Dec(_, Past.Region, Past.List(_, r), value) -> 
      let split = function
      | [vx] -> Ast.MultiOp(op, (List.map (fun v -> let (expr, _) = 
        translate_expr (substitute (get_var v) (get_var vx) c) vars in expr) (cell_grid l)))
      | vx::vs -> let (nv, _) = translate_quantifier l q (Past.Dec(l, Past.Region, Past.List(l, [vx]), value)) g
        (Past.Quantifier(l, q, (Past.Dec(l, Past.Region, Past.List(l, vs), value)), g, c)) vars
        in nv
      in let (reg, _) = translate_expr d vars in
      (Ast.Bundle(reg::[split r]), vars)
      
and translate_region_term e1 rop e2 vars =
  let (Ast.Var(v1), vars1) = translate_expr e1 vars in
  let (Ast.Var(v2), vars2) = translate_expr e2 vars1 in
  let rc1 = Ast.Var(sprintf "%sTo%s" v1 v2) in
  let rc2 = Ast.Var(sprintf "%sTo%s" v2 v1) in
  let f = (get_vars (create_vars ())) in
  if List.mem (rc1, rc2) f or List.mem (rc2, rc1) f then 
    match rop with
    | Past.Adjacent -> (Ast.Op(Ast.Op(Ast.Var(sprintf "%s_root" v1), Ast.Unequal, Ast.Var(sprintf "%s_root" v2)),
      Ast.LeftImp, Ast.UnaryOp(Ast.Not, Ast.Op(rc1, Ast.Or, rc2))), vars2)
  else (Ast.Dead, vars2)

and translate_utils e u vars =
  match u with
  | Past.Size -> (Ast.Var(sprintf "%s_size" (get_var e)), vars)
  | Past.Reg -> (Ast.Var(sprintf "%s_root" (get_var e)), vars)
  | Past.Sum -> (Ast.Var(sprintf "%s_sum" (get_var e)), vars)

and translate_range rc1 rc2 vars =
  match rc1, rc2 with
  | Past.RC(l, r1, c1), Past.RC(_, r2, c2) -> 
    let x1 = max (get_int r1) (get_int r2) in
    let y1 = max (get_int c1) (get_int c2) in
    let x2 = min (get_int r1) (get_int r2) in
    let y2 = min (get_int c1) (get_int c2) in
    let rec gen x y =
      if x = x2 then 
        if y = y2 then [(x, y)]
        else (x, y)::(gen x1 (y-1))
      else (x, y)::(gen (x-1) y)
    in (List.map (fun (r, c) -> (Past.RC(l, Past.Var(l, string_of_int r), Past.Var(l, string_of_int c)))) (gen x1 y1), vars)

and translate_expr e vars = 
  match e with
  | Past.Integer(_, n) -> (Ast.Integer(n), vars)
  | Past.Boolean(_, b) -> (Ast.Boolean(b), vars)
  | Past.RC(_, r, c) -> (Ast.Var(sprintf "r%ic%i" (get_int r) (get_int c)), vars)
  | Past.Var(_, v) -> (Ast.Var(v), vars)
  | Past.Op(_, e1, op, e2) -> 
    (match op with
      | Past.RightImp -> translate_term e2 Past.LeftImp e1 vars
      | Past.BiImp -> translate_term e1 Past.Equal e2 vars
      | _ -> translate_term e1 op e2 vars)
  | Past.UnaryOp(_, uop, e) -> translate_unary_term uop e vars
  | Past.RegionOp(_, e1, rop, e2) -> translate_region_term e1 rop e2 vars
  | Past.Dec(_, d, v, e) -> translate_dec d v e vars
  | Past.Quantifier(l, q, d, g, c) -> translate_quantifier l q d g c vars
  | Past.Utils(_, e, u) -> translate_utils e u vars
  | Past.List()

let rec clean = function
  | Ast.Op(e1, _, e2) -> clean e1 && clean e2
  | Ast.UnaryOp(_, e) -> clean e
  | Ast.Dec(_, e) -> clean e
  | Ast.ITE(e1, e2, e3) -> clean e1 && clean e2 && clean e3
  | Ast.Dead -> false
  | _ -> true

let rec flatten = function
  | Ast.Bundle(l)::es -> (flatten l) @ flatten es
  | Ast.MultiOp(op, l)::es -> Ast.MultiOp(op, flatten l)::flatten es
  | e::es -> if clean e then e::(flatten es) else flatten es
  | [] -> []

let convert = function
  | ((_, r, c), xs) -> gridr := r; gridc := c;
    let rec loop exprs vars = 
      match exprs with
      | e::es -> let (expr, nvars) = translate_expr e vars in
        expr :: (loop es nvars)
      | [] -> []
    in ((r, c), flatten (loop xs []))
