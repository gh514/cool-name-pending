
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
  | Past.Abs -> Ast.Abs

let get_loc = function
  | Past.Integer(l, _) -> l
  | Past.Boolean(l, _) -> l
  | Past.RC(l, _, _) -> l
  | Past.Var(l, _) -> l
  | Past.Op(l, _, _, _) -> l
  | Past.UnaryOp(l, _, _) -> l
  | Past.SpecOp(l, _, _, _) -> l
  | Past.Dec(l, _, _, _) -> l
  | Past.Assign(l, _, _) -> l
  | Past.Utils(l, _, _) -> l
  | Past.Quantifier(l, _, _, _, _) -> l
  | Past.List(l, _) -> l
  | Past.Group(l, _) -> l
  | Past.Range(l, _, _)
  | Past.Member(l, _, _) -> l
  | Past.Sugar(l, _, _, _) -> l

let get_int = function
  | Past.Integer(_, n) -> n

let get_bool = function
  | Past.Boolean(_, b) -> b

let get_var = function
  | Past.Var(_, v) -> v
  | Past.RC(_, Past.Integer(_, r), Past.Integer(_, c)) -> sprintf "r%ic%i" r c

let get_var_ast = function
  | Ast.Var(v) -> v

let rec substitute var_new var_old expr =
  let rec loop = function
    | e::es -> (substitute var_new var_old e) :: loop es
    | [] -> []
  in match expr with
    | Past.Var(l, v) -> if v = var_old then var_new else expr
    | Past.Op(l, e1, op, e2) -> Past.Op(l, substitute var_new var_old e1, op, substitute var_new var_old e2)
    | Past.UnaryOp(l, uop, e) -> Past.UnaryOp(l, uop, substitute var_new var_old e)
    | Past.SpecOp(l, e1, sop, e2) -> Past.SpecOp(l, substitute var_new var_old e1, sop, substitute var_new var_old e2)
    | Past.Dec(l, dt, v, Some def) -> Past.Dec(l, dt, v, Some (substitute var_new var_old def))
    | Past.Assign(l, v, e) -> Past.Assign(l, v, substitute var_new var_old e)
    | Past.Utils(l, e, u) -> Past.Utils(l, substitute var_new var_old e, u)
    | Past.Quantifier(l, q, d, g, e) -> Past.Quantifier(l, q, d, g, substitute var_new var_old e)
    | Past.List(l, ls) -> Past.List(l, loop ls)
    | Past.Group(_, g) -> (match g with
      | Past.Instance(e) -> substitute var_new var_old e
      | _ -> expr)
    | Past.Range(l, e1, e2) -> Past.Range(l, substitute var_new var_old e1, substitute var_new var_old e2)
    | Past.Member(l, e1, e2) -> Past.Member(l, substitute var_new var_old e1, substitute var_new var_old e2)
    | Past.Sugar(l, dt, g, c) -> Past.Sugar(l, dt, substitute var_new var_old g, c)
    | _ -> expr

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

let rows () = 
  let m = !gridr in
  let n = !gridc in
  let rec loop1 r = function
    | 0 -> []
    | c -> (r, c) :: loop1 r (c-1)
  in let rec loop2 = function
    | 0 -> []
    | r -> (loop1 r n) :: (loop2 (r-1))
  in loop2 m

let columns () = 
  let m = !gridr in
  let n = !gridc in
  let rec loop1 c = function
    | 0 -> []
    | r -> (r, c) :: loop1 c (r-1)
  in let rec loop2 = function
    | 0 -> []
    | c -> (loop1 c m) :: (loop2 (c-1))
  in loop2 m

let cell_grid l = List.map (fun (r, c) -> Past.RC(l, Past.Integer(l, r), Past.Integer(l, c))) (cells ())

let bool_grid reg = List.map (fun (r, c) -> Ast.Var(sprintf "r%ic%i_in_%s" r c reg)) (cells ())

let int_grid str = List.map (fun (r, c) -> Ast.Var(sprintf "r%ic%i_%s" r c str)) (cells ())

let rec unpair = function
  | (a, b)::ls -> a::b::(unpair ls)
  | [] -> []

let adj r c = 
  let m = !gridr in
  let n = !gridc in
  List.map (fun (rs, cs) -> ((r, c), (rs, cs)))
  (List.filter (fun (r, c) -> r >= 1 && r <= m && c >= 1 && c <= n) [(r+1, c); (r, c+1); (r, c-1); (r-1, c)])

let adj_diag x r c =
  let m = !gridr in
  let n = !gridc in
  List.map (fun (rs, cs) -> ((r, c), (rs, cs)))
  (List.filter (fun (r, c) -> r >= x && r <= m && c >= x && c <= n) 
    [(r, c-1); (r-1, c-1); (r-1, c); (r-1, c+1)])

let create_vars adj_f =
  let m = !gridr in
  let n = !gridc in
  let rec grid_pairs r c offset =
      match r, c with
      | 0, _ -> []
      | -1, _ -> []
      | _, 0 -> grid_pairs (r-1) (n-offset) (1-offset)
      | _, -1 -> grid_pairs (r-1) (n-offset) (1-offset)
      | _, _ -> (adj_f r c) @ (grid_pairs r (c-2) offset)
  in grid_pairs m n 1

let create_linevars x =
  let m = !gridr in
  let n = !gridc in
  let rec grid_pairs r c =
    if r = x-1 then []
    else if c = x-1 then grid_pairs (r-1) n
    else (adj_diag x r c) @ (grid_pairs r (c-1))
  in grid_pairs m n

let get_vars pairs = List.map (fun ((r1, c1), (r2, c2)) -> 
  (make_var_pair r1 c1 r2 c2, make_var_pair r2 c2 r1 c1)) pairs

let create_centreline l = 
  let Ast.Var(nl) = l in 
  (List.map (fun (r, c) -> Ast.Dec(Ast.Bool, Ast.Var(sprintf "%s_r%ic%i" nl r c))) (cells ()))
    @ (List.map (fun ((r1, c1), (r2, c2)) ->
    Ast.Dec(Ast.Bool, Ast.Var(sprintf "%s_r%ic%i-r%ic%i" nl r1 c1 r2 c2))) (create_linevars 1))

let create_edgeline l =
  let Ast.Var(nl) = l in
  (List.map (fun ((r1, c1), (r2, c2)) -> 
    Ast.Dec(Ast.Bool, Ast.Var(sprintf "%s_r%i.5c%i.5-r%i.5c%i.5" nl r1 c1 r2 c2))) (create_linevars 0))

let direction_constraints f = 
  let rec loop = function
    | (v1, v2)::ls -> Ast.UnaryOp(Ast.Not, Ast.Op(v1, Ast.And, v2)) :: (loop ls)
    | [] -> []
  in loop f

let rec constr_loop r c func =
  match (r, c) with
    | 0, _ -> []
    | _, 0 -> constr_loop (r-1) (!gridc) func
    | _, _ -> (func r c) @ constr_loop r (c-1) func

let get_parent_constraints () = 
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
    ))) adj_cells

  in constr_loop !gridr !gridc parent_constraints

let get_children_constraints () =
  let children_constraints r c =
    let adj_cells = adj r c in
    [Ast.Op(Ast.Var(sprintf "r%ic%i_count" r c), Ast.Equal,
      Ast.MultiOp(Ast.Add, Ast.Integer(1) ::
        (let rec loop = function
          | ((_, _), (ry, cy))::ls -> Ast.ITE(
            make_var_pair r c ry cy, Ast.Var(sprintf "r%ic%i_count" ry cy), Ast.Integer(0)) :: loop ls
          | [] -> []
        in loop adj_cells
        )))]
  in constr_loop !gridr !gridc children_constraints

let rec size_constraints = function
  | ((r1, c1), (r2, c2)) :: ls -> Ast.Op(Ast.Op(make_var_pair r1 c1 r2 c2, Ast.Or, make_var_pair r2 c2 r1 c1), Ast.LeftImp, 
    Ast.Op(Ast.Var(sprintf "r%ic%i_size" r1 c1), Ast.Equal, Ast.Var(sprintf "r%ic%i_size" r2 c2))) :: (size_constraints ls)
  | [] -> []

let rec origin_constraints = function
  | ((r1, c1), (r2, c2)) :: ls -> Ast.Op(Ast.Op(make_var_pair r1 c1 r2 c2, Ast.Or, make_var_pair r2 c2 r1 c1), Ast.LeftImp, 
    Ast.Op(Ast.Var(sprintf "r%ic%i_root" r1 c1), Ast.Equal, Ast.Var(sprintf "r%ic%i_root" r2 c2)))::(origin_constraints ls)
  | [] -> []

let get_root_constraints () =
  let root_constraints r c =
    [Ast.Op(Ast.UnaryOp(Ast.Not, Ast.MultiOp(Ast.Or, 
      List.map (fun ((_, _), (rx, cx)) -> make_var_pair rx cx r c) (adj r c))), Ast.LeftImp,
      Ast.MultiOp(Ast.And, [Ast.Op(Ast.Var(sprintf "r%ic%i_size" r c), Ast.Equal, Ast.Var(sprintf "r%ic%i_count" r c));
        Ast.Op(Ast.Var(sprintf "r%ic%i_root" r c), Ast.Equal, Ast.Integer((r-1)*(!gridc) + c));
        Ast.Op(Ast.Var(sprintf "r%ic%i_num" r c), Ast.Equal, Ast.Var(sprintf "r%ic%i_sum" r c))]))]
  in constr_loop !gridr !gridc root_constraints

let rec sum_constraints = function
  | ((r1, c1), (r2, c2)) :: ls -> Ast.Op(Ast.Op(make_var_pair r1 c1 r2 c2, Ast.Or, make_var_pair r2 c2 r1 c1), Ast.LeftImp, 
    Ast.Op(Ast.Var(sprintf "r%ic%i_sum" r1 c1), Ast.Equal, Ast.Var(sprintf "r%ic%i_sum" r2 c2)))::(sum_constraints ls)
  | [] -> []

let get_total_constraints () =
  let m = !gridr in
  let n = !gridc in
  
  let total_constraints r c =
    let adj_cells = adj r c in
    [Ast.Op(Ast.Var(sprintf "r%ic%i_num" r c), Ast.Equal,
      Ast.MultiOp(Ast.Add, Ast.Var(sprintf "r%ic%i" r c) ::
        (let rec loop = function
          | ((_, _), (ry, cy))::ls -> Ast.ITE(make_var_pair r c ry cy, Ast.Var(sprintf "r%ic%i_num" ry cy), Ast.Integer(0)) :: loop ls
          | [] -> []
        in loop adj_cells)))]
  in constr_loop !gridr !gridc total_constraints

let init_regions _ =
  let grid = create_vars adj in
  let field = List.map (fun v -> Ast.Dec(Ast.Bool, v)) (unpair (get_vars grid)) in
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
    field @ size_grid @ count_grid @ root_grid @ num_grid @ sum_grid @ constr_field @ parent_constr
     @ children_constr @ size_constr @ root_constr @ origin_constr @ sum_constr @ total_constr

let rec unpack_range = function
  | Past.Range(_, rc1, e2) -> rc1 :: (unpack_range e2)
  | x -> [x]

and unpack_line = function
  | e::es -> (unpack_range e) :: (unpack_line es)
  | [] -> []

let rec pair = function
  | rc1::rc2::xs -> (rc1, rc2) :: (rc2, rc1) :: (pair (rc2::xs))
  | _ -> []

let translate_centreline l v =
  let past_lines = unpack_line l
  in let lines = List.map (List.map (fun (Past.RC(_, r, c)) -> (get_int r, get_int c))) past_lines
  in (List.map (fun (r, c) -> let t = Ast.Var(sprintf "%s_r%ic%i" v r c)
    in if List.mem (r, c) (List.concat lines) then t else Ast.UnaryOp(Ast.Not, t)) (cells ())
    @ List.map (fun ((r1, c1), (r2, c2)) -> (let t = Ast.Var(sprintf "%s_r%ic%i-r%ic%i" v r1 c1 r2 c2)
    in if List.mem ((r1, c1), (r2, c2)) (List.concat_map pair lines) then t else Ast.UnaryOp(Ast.Not, t))) (create_vars (adj_diag 1)), 
    Past.List(get_loc (List.hd l), List.map (fun (rc1, rc2) -> Past.Range(get_loc rc1, rc1, rc2)) (List.concat_map pair past_lines)))

let translate_edgeline l v =
  let past_lines = unpack_line l
  in let lines = List.map (List.map (fun (Past.RC(_, Past.Corner(_, r), Past.Corner(_, c))) -> (get_int r, get_int c))) past_lines
  in (List.map (fun ((r1, c1), (r2, c2)) -> (let t = Ast.Var(sprintf "%s_r%i.5c%i.5-r%i.5c%i.5" v r1 c1 r2 c2)
    in if List.mem ((r1, c1), (r2, c2)) (List.concat_map pair lines) then t else Ast.UnaryOp(Ast.Not, t))) (create_linevars 0), 
    Past.List(get_loc (List.hd l), List.map (fun (rc1, rc2) -> Past.Range(get_loc rc1, rc1, rc2)) (List.concat_map pair past_lines)))

let expand_range rc1 rc2 =
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
    in List.map (fun (r, c) -> (Past.RC(l, Past.Integer(l, r), Past.Integer(l, c)))) (gen x1 y1)

let rec expand_list xs = 
  let rec loop = function
    | (Past.Range(l, x, y))::es -> (expand_list (expand_range x y)) @ (loop es)
    | z::es -> z :: loop es
    | [] -> []
  in loop xs

let define_region l init nv vars = 
  let t = expand_list l
  in let cells = List.map (fun (Past.RC(_, r, c)) -> (get_int r, get_int c)) t
  in let (r_root, c_root) = List.hd cells in (Ast.Bundle(init@[Ast.MultiOp(Ast.And, List.map (fun (r, c) -> 
    Ast.Op(Ast.Var(sprintf "r%ic%i_root" r c), Ast.Equal, Ast.Integer(!gridc*(r_root-1)+c_root))) cells)])
    , (Past.Region, nv, Some (Past.List(get_loc (List.hd t), t)))::vars)

let define_centreline l init nv vars =
  let Past.Var(_, v1) = nv in
  let (lines, nvars) = translate_centreline l v1 
  in (Ast.Bundle(init@lines), (Past.CentreLine, nv, Some nvars)::vars)

let define_edgeline l init nv vars = 
  let Past.Var(_, v1) = nv in
  let (lines, nvars) = translate_edgeline l v1
  in (Ast.Bundle(init@lines), (Past.EdgeLine, nv, Some nvars)::vars)

let define_box l nv vars = (Ast.Dead, (Past.Box, nv, Some (Past.List(get_loc (List.hd l), expand_list l)))::vars)

let rec find v = function
  | (dt, nv, e)::nvars -> let Past.Var(_, v1) = v in let Past.Var(_, v2) = nv in if v1 = v2 then (dt, e) else find v nvars
  | [] -> let Past.Var(_, v1) = v in raise (Err (sprintf "Variable %s not declared" v1))

let get_centreline_cells l = 
  let rec loop = function
    | Past.Range(_, Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)), Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)))::cs -> 
      [(r1, c1); (r2, c2)]@(loop cs)
    | [] -> []
  in List.fold_left (fun xs -> fun x -> if List.mem x xs then xs else x::xs) [] (loop l)

let type_spec_op e vars = 
  match e with
    | Past.Integer(_, _) -> Past.Int
    | Past.Boolean(_, _) -> Past.Bool
    | Past.RC(_, _, _) -> Past.Cell
    | Past.Var(l, v) -> let (dt, _) = find (Past.Var(l, v)) vars in dt
    | Past.Member(_, _, _) -> raise (Err "very bad")
    | _ -> raise (Err "Invalid operand")

let rec replace_spec_op expr vars = 
  match expr with
    | Past.Op(l, e1, op, e2) -> Past.Op(l, replace_spec_op e1 vars, op, replace_spec_op e2 vars)
    | Past.UnaryOp(l, uop, e) -> Past.UnaryOp(l, uop, replace_spec_op e vars)
    | Past.SpecOp(l, e1, sop, e2) -> (match type_spec_op e1 vars, sop, type_spec_op e2 vars with
      | Past.Cell, Past.Adjacent(None), Past.Cell -> Past.SpecOp(l, e1, Past.CellAdjacent, e2)
      | Past.Region, Past.Adjacent(None), Past.Region -> Past.SpecOp(l, e1, Past.RegionAdjacent, e2)
      | Past.Cell, Past.Adjacent(Some ls), Past.Cell -> Past.SpecOp(l, e1, Past.LineAdjacent(ls), e2)
      | Past.EdgeLine, Past.Adjacent(None), Past.Cell -> Past.SpecOp(l, e1, Past.CellLineAdjacent(-1), e2)
      | Past.Cell, Past.Adjacent(None), Past.EdgeLine -> Past.SpecOp(l, e1, Past.CellLineAdjacent(-1), e2)
      | _ -> expr)
    | Past.Dec(l, dt, v, e) -> (match e with
      | Some e1 -> Past.Dec(l, dt, v, Some (replace_spec_op e1 vars))
      | None -> expr)
    | Past.Assign(l, e1, e2) -> Past.Assign(l, replace_spec_op e1 vars, replace_spec_op e2 vars)
    | Past.Utils(l, e, u) -> Past.Utils(l, replace_spec_op e vars, u)
    | Past.Quantifier(l, q, d, g, c) -> Past.Quantifier(l, q, replace_spec_op d vars, g, replace_spec_op c vars)
    | Past.List(l, ls) -> Past.List(l, List.map (fun e -> replace_spec_op e vars) ls)
    | Past.Group(l, g) -> (match g with
      | Past.Instance(e) -> Past.Group(l, Past.Instance(replace_spec_op e vars))
      | _ -> expr)
    | Past.Range(l, e1, e2) -> Past.Range(l, replace_spec_op e1 vars, replace_spec_op e2 vars)
    | Past.Member(l, e1, e2) -> Past.Member(l, replace_spec_op e1 vars, replace_spec_op e2 vars)
    | Past.Sugar(l, dt, e1, c) -> Past.Sugar(l, dt, replace_spec_op e1 vars, c)
    | _ -> expr

let rec store_vars dt vars = function
  | v::vs -> store_vars dt ((dt, v, None)::vars) vs
  | [] -> vars

let rec translate_term e1 op e2 vars =
  let (expr1, vars1) = translate_expr e1 vars in
  let (expr2, vars2) = translate_expr e2 vars1 in
  (Ast.Op(expr1, translate_op op, expr2), vars2)

and translate_unary_term uop e vars =
  let (expr, _) = translate_expr e vars in
  (Ast.UnaryOp(translate_unary_op uop, expr), vars)

and translate_spec_term l e1 sop e2 vars =
  let sop1 = match e1, e2 with
    | Past.Var(_, _), _ -> let Past.SpecOp(_, _, nsop, _) = replace_spec_op (Past.SpecOp(l, e1, sop, e2)) vars in nsop
    | _, Past.Var(_, _) -> let Past.SpecOp(_, _, nsop, _) = replace_spec_op (Past.SpecOp(l, e1, sop, e2)) vars in nsop
    | _, _ -> sop
  in let (Ast.Var(v1), vars1) = translate_expr e1 vars in
  let (Ast.Var(v2), vars2) = translate_expr e2 vars1 in
  let rc1 = Ast.Var(sprintf "%sTo%s" v1 v2) in
  let rc2 = Ast.Var(sprintf "%sTo%s" v2 v1) in

  match sop1 with

    | Past.RegionAdjacent -> let Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)) = e1 in 
      let Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)) = e2 in
      ((if (abs (r1-r2) + abs (c1-c2)) = 1 then 
      Ast.Op(Ast.Var(sprintf "%s_root" v1), Ast.Unequal, Ast.Var(sprintf "%s_root" v2)) else Ast.Dead), vars2)

    | Past.CellAdjacent -> let Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)) = e1 in 
      let Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)) = e2 in
      ((if (abs (r1-r2+c1-c2)) <= 1 then Ast.Boolean(true) else Ast.Boolean(false)), vars2)

    | Past.LineAdjacent(v) -> 
      let Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)) = e1 in 
      let Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)) = e2 in
      ((if abs (r1-r2) <= 1 && abs (c1-c2) <= 1 then
      (let (_, Some (Past.List(_, ls))) = find v vars in
      let rec loop = function
        | Past.Range(_, Past.RC(_, Past.Integer(_, ra1), Past.Integer(_, ca1)), 
          Past.RC(_, Past.Integer(_, ra2), Past.Integer(_, ca2)))::es -> 
          if (r1, c1, r2, c2) = (ra1, ca1, ra2, ca2) || (r1, c1, r2, c2) = (ra2, ca2, ra1, ca1)
          then Ast.Boolean(true) else loop es
        | [] -> Ast.Boolean(false)
      in loop ls) else Ast.Dead), vars2)

    | Past.CellLineAdjacent(n) -> 
      let constr r c v = List.map (fun ((r1, c1), (r2, c2)) -> Ast.Var(sprintf "%s_r%i.5c%i.5-r%i.5c%i.5" v r1 c1 r2 c2)) 
        [((r, c), (r-1, c)); ((r, c), (r, c-1)); ((r-1, c), (r-1, c-1)); ((r, c-1), (r-1, c-1))]
      in let num_constr r c v n = 
        (if n = -1 then Ast.MultiOp(Ast.Or, constr r c v)
        else Ast.Op(Ast.MultiOp(Ast.Add, List.map (fun var -> Ast.ITE(var, Ast.Integer(1), Ast.Integer(0))) (constr r c v))
          , Ast.Equal, Ast.Integer(n)))
      in ((match e1, e2 with
        | Past.Var(_, v), Past.RC(_, Past.Integer(_, r), Past.Integer(_, c)) -> num_constr r c v n
        | Past.RC(_, Past.Integer(_, r), Past.Integer(_, c)), Past.Var(_, v) -> num_constr r c v n)


        
        , vars2)

      

    | Past.Adjacent(_) -> raise (Err "Adj operation not substituted")

and translate_dec d v e vars = 
  let helper v1 vars =
    let (nv, _) = translate_expr v1 vars in
    match d with
    | Past.Int -> (Ast.Dec(Ast.Int, nv), (Past.Int, v1, None)::vars)
    | Past.Cell -> (match e with 
      | None -> (Ast.Dead, (Past.Cell, v1, None)::vars))
    | Past.Region -> let init = if !regions then [] else (regions := true; init_regions ()) in
      (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_region l init v1 vars
      | None -> (Ast.Bundle(init), (Past.Region, v1, None)::vars))
    | Past.CentreLine -> let init = create_centreline nv in 
      (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_centreline l init v1 vars
      | None -> (Ast.Bundle(init), (Past.CentreLine, v1, None)::vars))
    | Past.EdgeLine -> let init = create_edgeline nv in
      (match e with
        | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_edgeline l init v1 vars
        | None -> (Ast.Bundle(init), (Past.EdgeLine, v1, None)::vars))
    | Past.Box -> (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_box l v1 vars)
    | _ -> raise (Err "Unimplemented datatype")
  in let rec loop vars = function
    | v::vs -> let (ne, nvars) = helper v vars in (ne, nvars)::(loop nvars vs)
    | [v] -> [helper v vars]
    | [] -> []
  in match v with 
  | Past.List(_, l) -> let nl = loop vars l in
    let (es, vs) = List.split nl in (Ast.Bundle(es), List.nth vs ((List.length vs)-1))

and translate_assignment v e vars = 
  let (v1, _) = translate_expr v vars
  in let Past.Group(_, g) = e 
  in match find v vars with
    | Past.Region, _ -> let Past.Instance(Past.List(_, l)) = g in define_region l [] v vars
    | Past.CentreLine, _ -> let Past.Instance(Past.List(_, l)) = g in 
      let (line, nvars) = translate_centreline l (get_var v)
      in (Ast.Bundle(line), (Past.CentreLine, v, Some nvars)::vars)

and translate_utils e u vars =
  match u with
  | Past.Size -> (match e with
    | Past.Utils(_, e1, Past.Reg) -> translate_utils e1 u vars
    | _ -> (Ast.Var(sprintf "%s_size" (get_var e)), vars))
  | Past.Reg -> (Ast.Var(sprintf "%s_root" (get_var e)), vars)
  | Past.Sum -> (Ast.Var(sprintf "%s_sum" (get_var e)), vars)
    
and translate_quantifier l q d g c vars =
  match q with
    | Past.NForAll -> translate_expr (Past.UnaryOp(l, Past.Not, Past.Quantifier(l, q, d, g, c))) vars
    | Past.NExists -> translate_expr (Past.UnaryOp(l, Past.Not, Past.Quantifier(l, q, d, g, c))) vars
    | _ -> let op = match q with
        | Past.ForAll -> Ast.And
        | Past.Exists -> Ast.Or
      in match d with
        | Past.Dec(_, Past.Cell, Past.List(_, ls), value) -> 
          let split = function
            | [vx] -> Ast.MultiOp(op, (List.map (fun v -> let (expr, _) = 
              translate_expr (substitute v (get_var vx) c) vars in expr) (translate_group l g vars)))
            | vx::vs -> let (nv, _) = translate_quantifier l q (Past.Dec(l, Past.Cell, Past.List(l, vs), value)) g
                (Past.Quantifier(l, q, (Past.Dec(l, Past.Cell, Past.List(l, [vx]), value)), g,
                replace_spec_op c (store_vars Past.Cell vars ls))) vars
              in nv
          in (split ls, vars)
        | Past.Dec(_, Past.Region, Past.List(_, ls), value) -> 
          let split = function
            | [vx] -> Ast.MultiOp(op, (List.map (fun v -> let (expr, _) = 
              translate_expr (substitute v (get_var vx) c) vars in expr) (cell_grid l)))
            | vx::vs -> let (nv, _) = translate_quantifier l q (Past.Dec(l, Past.Region, Past.List(l, vs), value)) g
              (Past.Quantifier(l, q, (Past.Dec(l, Past.Region, Past.List(l, [vx]), value)), g, 
              replace_spec_op c (store_vars Past.Region vars ls))) vars
              in nv
          in let (reg, _) = translate_expr d vars 
          in (Ast.Bundle(reg::[split ls]), vars)

and translate_group l g vars = match g with
  | Past.Grid -> cell_grid l
  | Past.Instance(e) -> (match e with
    | Past.List(_, es) -> (expand_list es)
    | Past.Var(_, v) -> let (dt, Some ls) = find e vars
      in match dt, ls with
        | Past.Region, Past.List(_, list) -> list
        | Past.CentreLine, Past.List(_, list) -> List.map (fun (r, c) -> Past.RC(l, Past.Integer(l, r), Past.Integer(l, c)))
          (get_centreline_cells list))
        
and translate_list ls vars = 
  let rec loop = function
    | e::es -> let (v, _) = translate_expr e vars in v :: loop es
    | [] -> []
  in (Ast.Bundle(loop ls), vars)

and translate_member e1 e2 vars = 
  let (Ast.Var(v2), vars2) = translate_expr e2 vars
  in match e1 with
    | Past.List(_, l) -> 
      (let rec var_loop vars3 = function
        | e::es -> let (_, vars1) = translate_expr e vars3 in var_loop vars1 es
        | [] -> vars
      in let rec loop = function
        | e::es -> let (Ast.Var(v1), _) = translate_expr e vars in
          (Ast.Var(sprintf "%s_%s" v2 v1)) :: (loop es)
        | [] -> []
      in (Ast.Bundle(loop l), var_loop vars2 l))
    | _ -> let (Ast.Var(v1), _) = translate_expr e1 vars in (Ast.Var(sprintf "%s_%s" v2 v1), vars2)

and translate_sugar dt e c vars =
  let con = match c with 
    | Past.Distinct -> Ast.Unequal
    | Past.Equivalent -> Ast.Equal
  in let to_var = List.map (fun (r, c) -> Ast.Var(sprintf "r%ic%i" r c))
  in let Past.Group(_, g) = e 
  in match dt with
    | Past.Cell -> (match g with
      | Past.Grid -> (Ast.MultiOp(con, to_var (cells ())), vars)
      | Past.Row -> (Ast.MultiOp(Ast.And, List.map (fun l -> Ast.MultiOp(con, to_var l)) (rows ())), vars)
      | Past.Column -> (Ast.MultiOp(Ast.And, List.map (fun l -> Ast.MultiOp(con, to_var l)) (columns ())), vars)
      | Past.Boxes -> (Ast.MultiOp(Ast.And, (List.filter_map (fun (dt, v, Some e) ->
        match e with
          | Past.List(_, ls) -> 
            if dt = Past.Box then Some (Ast.MultiOp(con, List.map (fun (Past.RC(_, Past.Integer(_, r), Past.Integer(_, c))) -> 
            Ast.Var(sprintf "r%ic%i" r c)) ls)) else None
          | _ -> None) vars)), vars)
      | Past.Regions -> (Ast.MultiOp(Ast.And, (List.filter_map (fun (dt, v, Some e) ->
        match e with
          | Past.List(_, ls) -> if dt = Past.Region then 
              Some (Ast.MultiOp(con, List.map (fun (Past.RC(_, Past.Integer(_, r), Past.Integer(_, c))) -> 
              Ast.Var(sprintf "r%ic%i" r c)) ls)) else None
          | _ -> None) vars)), vars)
      | Past.Instance(v) -> (let (_, Some e) = find v vars in 
        match e with
          | Past.List(_, ls) -> Ast.MultiOp(Ast.Unequal, List.map 
            (fun (Past.RC(_, Past.Integer(_, r), Past.Integer(_, c))) -> Ast.Var(sprintf "r%ic%i" r c)) ls), vars))

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
  | Past.SpecOp(l, e1, sop, e2) -> translate_spec_term l e1 sop e2 vars
  | Past.Dec(_, d, v, e) -> translate_dec d v e vars
  | Past.Assign(_, v, e) -> translate_assignment v e vars
  | Past.Utils(_, e, u) -> translate_utils e u vars
  | Past.Quantifier(l, q, d, g, c) -> translate_quantifier l q d g c vars
  | Past.List(_, l) -> translate_list l vars
  | Past.Member(_, e1, e2) -> translate_member e1 e2 vars
  | Past.Sugar(_, dt, e, c) -> translate_sugar dt e c vars 

let init_named_regions vars =
  let rec unpack = function
    | (Past.RC(_, Past.Integer(_, r), Past.Integer(_, c)))::es -> (r, c)::(unpack es)
    | [] -> []
  in let rec loop = function
    | (Past.Region, Past.Var(_, v), None)::es -> 
      (List.map (fun (r, c) -> Ast.Dec(Ast.Bool, Ast.Var(sprintf "%s_r%ic%i" v r c))) (cells ()))@(loop es)
    | (Past.Region, Past.Var(_, v), Some (Past.List(_, l)))::es -> 
      (List.map (fun (r, c) -> Ast.Dec(Ast.Bool, Ast.Var(sprintf "%s_r%ic%i" v r c))) (cells ()))
      @ (List.map (fun (r, c) -> Ast.Var(sprintf "%s_r%ic%i" v r c)) (unpack l))@(loop es)
    | _::es -> loop es
    | [] -> []
  in Ast.Bundle(loop vars)

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

let replace l =
  let rec loop = function
    | [x] -> []
    | x::xs -> x::(loop xs)
  in (List.nth l ((List.length l)-1))::(loop l)

let convert = function
  | ((_, r, c), xs) -> gridr := r; gridc := c;
    let rec loop exprs vars = 
      match exprs with
      | e::es -> let (expr, nvars) = translate_expr e vars in
        expr :: (loop es nvars)
      | [] -> [init_named_regions vars]
    in ((r, c), flatten (replace (loop xs [])))
