
include Printf

exception Err of string
exception RegVar of Past.expr
exception UndecVar
exception Internal
exception NotVar

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
  | Past.CellDec(l, _) -> l
  | Past.ITE(l, _, _, _) -> l

let get_int = function
  | Past.Integer(_, n) -> n
  | _ -> raise (Err "Integer expected")

let get_bool = function
  | Past.Boolean(_, b) -> b
  | _ -> raise (Err "Boolean expected")

let get_var = function
  | Past.Var(_, v) -> v
  | Past.RC(_, Past.Integer(_, r), Past.Integer(_, c)) -> sprintf "r%ic%i" r c
  | _ -> raise (Err "Variable expected")

let rec find v vars =
  let v1 = (match v with
    | Past.Var(_, v1) -> v1
    | _ -> raise NotVar)
  in match vars with
    | (dt, nv, e)::nvars -> (match nv with
      | Past.Var(_, v2) -> if v1 = v2 then (dt, e) else find v nvars
      | _ -> raise Internal)
    | [] -> raise UndecVar

let safe_find e vars = try find e vars with UndecVar -> raise (Err (sprintf "Variable %s not initialised" (get_var e)))

let rec eval_num e vars = match e with
  | Past.Integer(_, i) -> i
  | Past.Var(_) -> let (_, Some e2) = safe_find e vars in eval_num e2 vars 
  | Past.Op(_, e1, op, e2) -> (match op with
    | Past.Add -> eval_num e1 vars + eval_num e2 vars
    | Past.Sub -> eval_num e1 vars - eval_num e2 vars
    | Past.Mul -> eval_num e1 vars * eval_num e2 vars
    | Past.Div -> eval_num e1 vars / eval_num e2 vars
    | _ -> raise (Err "Invalid operation"))
  | Past.UnaryOp(_, uop, e1) -> (match uop with
    | Past.Neg -> - eval_num e1 vars
    | Past.Abs -> abs (eval_num e1 vars))
  | _ -> raise (Err "Invalid operand")

let remove_duplicates = List.fold_left (fun xs -> fun x -> if List.mem x xs then xs else x::xs) [] 

let get_rc e vars = match e with
  | Past.RC(_, r, c) -> 
    let check_rc x limit = if 1 <= x && x <= limit then x else raise (Err "Invalid index")
    in let f e2 limit =
     match e2 with
      | Past.Integer(_, i) -> check_rc i limit
      | Past.Var(_, v) -> let (_, x) = safe_find e2 vars
        in (match x with
          | Some (Past.Integer(_, i)) -> check_rc i limit
          | Some (_) -> raise (Err "Invalid type")
          | None -> raise (RegVar e2))
      | _ -> eval_num e2 vars
    in (f r !gridr, f c !gridc)

let rec substitute var_new var_old expr =
  let rec loop = function
    | e::es -> (helper e) :: loop es
    | [] -> []
  and helper expr = match expr with
    | Past.Var(_, v) -> if v = var_old then var_new else expr
    | Past.Op(l, e1, op, e2) -> Past.Op(l, helper e1, op, helper e2)
    | Past.UnaryOp(l, uop, e) -> Past.UnaryOp(l, uop, helper e)
    | Past.SpecOp(l, e1, sop, e2) -> Past.SpecOp(l, helper e1, sop, helper e2)
    | Past.Dec(l, dt, v, Some def) -> Past.Dec(l, dt, v, Some (helper def))
    | Past.Assign(l, v, e) -> Past.Assign(l, v, helper e)
    | Past.Utils(l, e, u) -> Past.Utils(l, helper e, u)
    | Past.Quantifier(l, q, d, g, e) -> Past.Quantifier(l, q, d, g, helper e)
    | Past.List(l, ls) -> Past.List(l, loop ls)
    | Past.Group(_, g) -> (match g with
      | Past.Instance(e) -> helper e
      | _ -> expr)
    | Past.Range(l, e1, e2) -> Past.Range(l, helper e1, helper e2)
    | Past.Member(l, e1, e2) -> Past.Member(l, helper e1, helper e2)
    | Past.Sugar(l, dt, g, c) -> Past.Sugar(l, dt, helper g, c)
    | Past.CellDec(l, e) -> Past.CellDec(l, helper e)
    | Past.ITE(l, e1, e2, e3) -> Past.ITE(l, helper e1, helper e2, helper e3)
    | _ -> expr
  in helper expr

let make_var_pair r1 c1 r2 c2 = Ast.Var(sprintf "r%ic%itor%ic%i" r1 c1 r2 c2)

let get_cells x =
  let m = !gridr in
  let n = !gridc in
  let rec loop r c =
    if r = x then []
    else if c = x then loop (r-1) n
    else (r, c)::loop r (c-1)
  in loop m n

let row x = 
  let rec loop = function
    | 0 -> []
    | i -> (x, i)::loop (i-1)
  in loop !gridc

let column x =
  let rec loop = function
    | 0 -> []
    | i -> (i, x)::loop (i-1)
  in loop !gridr
  
let rows () = 
  let rec loop = function
    | 0 -> []
    | r -> row r :: loop (r-1)
  in loop !gridr

let columns () = 
  let rec loop = function
    | 0 -> []
    | c -> column c :: loop (c-1)
  in loop !gridc

let cell_grid l = List.map (fun (r, c) -> Past.RC(l, Past.Integer(l, r), Past.Integer(l, c))) (get_cells 0)

let bool_grid reg = List.map (fun (r, c) -> Ast.Var(sprintf "r%ic%i_in_%s" r c reg)) (get_cells 0)

let int_grid str = List.map (fun (r, c) -> Ast.Var(sprintf "r%ic%i_%s" r c str)) (get_cells 0)

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
      | _ -> (adj_f r c) @ (grid_pairs r (c-2) offset)
  in grid_pairs m n 1

let cell_pred rc1 rc2 =
  let (r1, c1), (r2, c2) = rc1, rc2
  in r1 < r2 || (r1 = r2 && (c1 < c2))

let cell_order rc1 rc2 =
  if cell_pred rc1 rc2 then (rc2, rc1) else (rc1, rc2)

let pair_line_segments l = 
  List.concat_map (fun rc1 -> List.map (fun rc2 -> (rc1, rc2)) (List.filter (fun rc3 -> cell_pred rc3 rc1) l)) l

let create_linevars x =
  let all = get_cells x
  in (List.concat_map (fun (r, c) -> (adj_diag (x+1) r c)) all, pair_line_segments all)

let get_vars pairs = List.map (fun ((r1, c1), (r2, c2)) -> 
  (make_var_pair r1 c1 r2 c2, make_var_pair r2 c2 r1 c1)) pairs

let surrounding x (r, c) =
  let m = !gridr in
  let n = !gridc in
  (List.filter (fun (r, c) -> r >= x && r <= m && c >= x && c <= n) 
    [(r+1, c+1); (r+1, c); (r+1, c-1);
     (r, c+1); (r, c-1);
     (r-1, c+1); (r-1, c); (r-1, c-1)])

let line_constraints l s x line = 
  let Ast.Var(nl) = l
  in let (adj_lines, linevars) = create_linevars (x-1)
  in let line_segments = List.concat_map (fun (rc1, rc2) -> [(rc1, rc2); (rc2, rc1)]) adj_lines
  in let to_var (r1, c1) (r2, c2) = Ast.Var(sprintf "%s_r%i%sc%i%stor%i%sc%i%s" nl r1 s c1 s r2 s c2 s )
  in let to_cell (r, c) ss = Ast.Var(sprintf "%s_r%i%sc%i%s%s" nl r s c s ss)
  in let cells = get_cells (x-1)
  in List.map (fun rc -> Ast.Dec(Ast.Bool, to_cell rc "")) cells
    @ List.map (fun (rc1, rc2) -> Ast.Dec(Ast.Bool, to_var rc1 rc2)) line_segments
    @ List.map (fun rc -> Ast.Dec(Ast.Bool, to_cell rc "_source")) cells
    @ List.map (fun rc -> Ast.Dec(Ast.Int, to_cell rc "_count")) cells
    @ (if line then [] else List.map (fun rc -> Ast.Dec(Ast.Int, to_cell rc "_sink")) cells)
    @ (if line then [] else [Ast.Op(Ast.MultiOp(Ast.Add, List.map (fun rc -> Ast.ITE(to_cell rc "_sink", Ast.Integer(1), Ast.Integer(0))) cells), Ast.Equal, Ast.Integer(1))])
    @ List.map (fun (rc1, rc2) -> Ast.Op(to_var rc1 rc2, Ast.LeftImp, Ast.Op(to_cell rc1 "", Ast.And, to_cell rc2 ""))) line_segments
    @ List.map (fun (rc1, rc2) -> Ast.Op(to_var rc1 rc2, Ast.LeftImp, Ast.UnaryOp(Ast.Not, to_var rc2 rc1))) line_segments
    @ List.map (fun (rc1, rc2) -> Ast.Op(Ast.Op(to_cell rc1 "", Ast.And, to_cell rc2 ""), Ast.LeftImp, Ast.MultiOp(Ast.Or, 
      List.map (fun rc -> Ast.Op(to_var rc1 rc, Ast.And, to_cell rc "")) (surrounding x rc1)
      @ List.map (fun rc -> Ast.Op(to_var rc2 rc, Ast.And, to_cell rc "")) (surrounding x rc2)))) linevars  (*many solutions vs forcing*)
    @ [Ast.Op(Ast.MultiOp(Ast.Add, List.map (fun rc -> Ast.ITE(to_cell rc "_source", Ast.Integer(1), Ast.Integer(0))) cells), Ast.Equal, Ast.Integer(1))]
    @ List.map (fun rc1 -> Ast.Op(to_cell rc1 "_source", Ast.LeftImp, Ast.UnaryOp(Ast.Not, 
      Ast.MultiOp(Ast.Or, List.map (fun rc2 -> to_var rc2 rc1) (surrounding x rc1))))) cells
    @ List.map (fun rc -> Ast.Op(to_cell rc "_source", Ast.LeftImp, (Ast.Op(to_cell rc "_count", Ast.Equal, Ast.Integer(0))))) cells
    @ List.map (fun (rc1, rc2) -> Ast.Op(to_var rc1 rc2, Ast.LeftImp, Ast.Op(to_cell rc1 "_count", Ast.LT, to_cell rc2 "_count"))) line_segments
    @ List.map (fun rc1 -> Ast.Op(Ast.MultiOp(Ast.Add, List.map (fun rc2 -> Ast.ITE(to_var rc1 rc2, Ast.Integer(1), Ast.Integer(0))) (surrounding x rc1))
      , Ast.LTE, Ast.ITE(to_cell rc1 "_source", Ast.Integer(2), Ast.Integer(1)))) cells
    @ List.map (fun rc1 -> Ast.Op(to_cell rc1 "_source", Ast.LeftImp, Ast.UnaryOp(Ast.Not, Ast.MultiOp(Ast.Or, 
      List.map (fun rc2 -> to_var rc2 rc1) (surrounding x rc1))))) cells
    @ (if line then List.map (fun rc1 -> Ast.Op(Ast.MultiOp(Ast.Add,
        List.map (fun rc2 -> Ast.ITE(to_var rc2 rc1, Ast.Integer(1), Ast.Integer(0))) (surrounding x rc1)), Ast.LTE, Ast.Integer(1)))
      else List.map (fun rc1 -> Ast.Op(Ast.UnaryOp(Ast.Not, to_cell rc1 "_sink"), Ast.LeftImp, Ast.Op(Ast.MultiOp(Ast.Add, 
        List.map (fun rc2 -> Ast.ITE(to_var rc2 rc1, Ast.Integer(1), Ast.Integer(0))) (surrounding x rc1)), Ast.LTE, Ast.Integer(1))))) cells
    @ (if line then [] else List.map (fun rc1 -> Ast.Op(to_cell rc1 "_sink", Ast.LeftImp, Ast.Op(Ast.MultiOp(Ast.Add, 
      List.map (fun rc2 -> Ast.ITE(to_var rc2 rc1, Ast.Integer(1), Ast.Integer(0))) (surrounding x rc1)), Ast.Equal, Ast.Integer(2)))) cells)

let create_centreline l = line_constraints l "" 1 true

let create_edgeline l = line_constraints l ".5" 0 true

let create_centreloop l = line_constraints l "" 1 false

let create_edgeloop l = line_constraints l ".5" 0 false

let direction_constraints f = 
  let rec loop = function
    | (v1, v2)::ls -> Ast.UnaryOp(Ast.Not, Ast.Op(v1, Ast.And, v2)) :: (loop ls)
    | [] -> []
  in loop f

let rec constr_loop r c func =
  match (r, c) with
    | 0, _ -> []
    | _, 0 -> constr_loop (r-1) (!gridc) func
    | _ -> (func r c) @ constr_loop r (c-1) func

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

let translate_centreline l v vars =
  let past_lines = unpack_line l
  in let lines = List.map (List.map (fun rc -> get_rc rc vars)) past_lines
  in let (adj_lines, _) = create_linevars 0
  in (List.map (fun (r, c) -> let t = Ast.Var(sprintf "%s_r%ic%i" v r c)
    in if List.mem (r, c) (List.concat lines) then t else Ast.UnaryOp(Ast.Not, t)) (get_cells 0)
    @ List.map (fun ((r1, c1), (r2, c2)) -> (let t = Ast.Var(sprintf "%s_r%ic%itor%ic%i" v r1 c1 r2 c2)
    in if List.mem ((r1, c1), (r2, c2)) (List.concat_map pair lines) then t else Ast.UnaryOp(Ast.Not, t))) adj_lines, 
      Past.List(get_loc (List.hd l), List.map (fun (rc1, rc2) -> Past.Range(get_loc rc1, rc1, rc2)) (List.concat_map pair past_lines)))

let translate_edgeline l v vars =
  let past_lines = unpack_line l
  in let lines = List.map (List.map (fun rc -> get_rc rc vars)) past_lines
  in let (adj_lines, _) = create_linevars (-1)
  in (List.map (fun ((r1, c1), (r2, c2)) -> (let t = Ast.Var(sprintf "%s_r%i.5c%i.5tor%i.5c%i.5" v r1 c1 r2 c2)
    in if List.mem ((r1, c1), (r2, c2)) (List.concat_map pair lines) then t else Ast.UnaryOp(Ast.Not, t))) adj_lines, 
    Past.List(get_loc (List.hd l), List.map (fun (rc1, rc2) -> Past.Range(get_loc rc1, rc1, rc2)) (List.concat_map pair past_lines)))

let expand_range vars (rc1, rc2) =
  let (r1, c1), (r2, c2) = get_rc rc1 vars, get_rc rc2 vars in
    let x1 = max r1 r2 in
    let y1 = max c1 c2 in
    let x2 = min r1 r2 in
    let y2 = min c1 c2 in
    let rec gen x y =
      if x = x2 then 
        if y = y2 then [(x, y)]
        else (x, y)::(gen x1 (y-1))
      else (x, y)::(gen (x-1) y)
    in (gen x1 y1)

let expand_list ls vars =
  let rec segments = function
    | rc1::rc2::l -> (rc1,rc2)::(segments (rc2::l))
    | [rc] -> [(rc, rc)]
    | _ -> []
  in let loc = get_loc (List.hd ls) in List.map (fun (r, c) -> Past.RC(loc, Past.Integer(loc, r), Past.Integer(loc, c))) 
    (remove_duplicates (List.concat_map (fun l -> List.concat_map (expand_range vars) (segments l)) (unpack_line ls)))

let define_region l init nv vars = 
  let t = expand_list l vars
  in let cells = List.map (fun rc -> get_rc rc vars) t
  in let (r_root, c_root) = List.hd cells
  in (Ast.Bundle(init@[Ast.MultiOp(Ast.And, List.map (fun (r, c) -> 
    let v = Ast.Op(Ast.Var(sprintf "r%ic%i_root" r c), Ast.Equal, Ast.Integer(!gridc*(r_root-1)+c_root)) 
    in if List.mem (r, c) cells then v else Ast.UnaryOp(Ast.Not, v)) (get_cells 0))])
    , (Past.Region, nv, Some (Past.List(get_loc (List.hd t), t)))::vars)

let define_centreline l init nv vars =
  let Past.Var(_, v1) = nv in
  let (lines, nvars) = translate_centreline l v1 vars
  in (Ast.Bundle(init@lines), (Past.CentreLine, nv, Some nvars)::vars)

let define_edgeline l init nv vars = 
  let Past.Var(_, v1) = nv in
  let (lines, nvars) = translate_edgeline l v1 vars
  in (Ast.Bundle(init@lines), (Past.EdgeLine, nv, Some nvars)::vars)

let define_box l nv vars = (Ast.Dead, (Past.Box, nv, Some (Past.List(get_loc (List.hd l), expand_list l vars)))::vars)

let get_centreline_cells l = 
  let rec loop = function
    | Past.Range(_, Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)), Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)))::cs -> 
      [(r1, c1); (r2, c2)]@(loop cs)
    | [] -> []
  in remove_duplicates (loop l)

let type_spec_op e vars = 
  match e with
    | Past.Integer(_, _) -> Past.Int
    | Past.Boolean(_, _) -> Past.Bool
    | Past.RC(_, _, _) -> Past.Cell
    | Past.Var(_) -> let (dt, _) = safe_find e vars in dt
    | _ -> raise (Err "Invalid operand")

let replace_spec_op expr vars = 
  let rec loop expr = match expr with
    | Past.Op(l, e1, op, e2) -> Past.Op(l, loop e1, op, loop e2)
    | Past.UnaryOp(l, uop, e) -> Past.UnaryOp(l, uop, loop e)
    | Past.SpecOp(l, e1, sop, e2) -> (match type_spec_op e1 vars, sop, type_spec_op e2 vars with
      | Past.Cell, Past.Adjacent(None), Past.Cell -> Past.SpecOp(l, e1, Past.CellAdjacent, e2)
      | Past.Region, Past.Adjacent(None), Past.Region -> Past.SpecOp(l, e1, Past.RegionAdjacent, e2)
      | Past.Cell, Past.Adjacent(Some ls), Past.Cell -> Past.SpecOp(l, e1, Past.LineAdjacent(ls), e2)
      | Past.EdgeLine, Past.Adjacent(None), Past.Cell -> Past.SpecOp(l, e1, Past.CellLineAdjacent(-1), e2)
      | Past.Cell, Past.Adjacent(None), Past.EdgeLine -> Past.SpecOp(l, e1, Past.CellLineAdjacent(-1), e2)
      | _ -> expr)
    | Past.Dec(l, dt, v, e) -> (match e with
      | Some e1 -> Past.Dec(l, dt, v, Some (loop e1))
      | None -> expr)
    | Past.Assign(l, e1, e2) -> Past.Assign(l, loop e1, loop e2)
    | Past.Utils(l, e, u) -> Past.Utils(l, loop e, u)
    | Past.Quantifier(l, q, d, g, c) -> Past.Quantifier(l, q, loop d, g, loop c)
    | Past.List(l, ls) -> Past.List(l, List.map (fun e -> loop e) ls)
    | Past.Group(l, g) -> (match g with
      | Past.Instance(e) -> Past.Group(l, Past.Instance(loop e))
      | _ -> expr)
    | Past.Range(l, e1, e2) -> Past.Range(l, loop e1, loop e2)
    | Past.Member(l, e1, e2) -> Past.Member(l, loop e1, loop e2)
    | Past.Sugar(l, dt, e1, c) -> Past.Sugar(l, dt, loop e1, c)
    | Past.CellDec(l, e) -> Past.CellDec(l, loop e)
    | Past.ITE(l, e1, e2, e3) -> Past.ITE(l, loop e1, loop e2, loop e3)
    | _ -> expr
  in loop expr

let scan_rc expr vars =
  let rec match_group = function
    | Past.Instance(e) -> loop e
    | _ -> []
  and loop e =
    match e with
      | Past.RC(_, e1, e2) -> 
        let f v x = function
          | None -> [(v, x)]
          | Some _ -> []
        in let m e x = match e with
          | Past.Var(_, v) -> let (_, e1) = safe_find e vars in f v x e1
          | _ -> []
        in m e1 (!gridr) @ m e2 (!gridc)
      | Past.Corner(_, e1) -> loop e1
      | Past.Op(_, e1, _, e2) -> loop e1 @ loop e2
      | Past.UnaryOp(_, _, e1) -> loop e1
      | Past.SpecOp(_, e1, _, e2) -> loop e1 @ loop e2
      | Past.Dec(_, _, e1, e2) -> loop e1 @ (match e2 with
        | Some e3 -> loop e3
        | None -> [])
      | Past.Assign(_, e1, e2) -> loop e1 @ loop e2
      | Past.Utils(_, e1, _) -> loop e1
      | Past.Quantifier(_, _, e1, g, e2) -> loop e1 @ loop e2 @ match_group g
      | Past.List(_, l) -> let f a b = loop a @ b in List.fold_right f l []
      | Past.Group(_, g) -> match_group g
      | Past.Range(_, e1, e2) -> loop e1 @ loop e2
      | Past.Member(_, e1, e2) -> loop e1 @ loop e2
      | Past.Sugar(_, _, e1, _) -> loop e1
      | Past.CellDec(_, e1) -> loop e1
      | Past.ITE(_, e1, e2, e3) -> loop e1 @ loop e2 @ loop e3
      | _ -> []
  in loop expr

let replace_rc vs expr = 
  let helper v x e =
    let rec loop e = 
      match e with
        | Past.RC(l, r, c) -> let f e1 = match e1 with
            | Past.Var(_, v1) -> if v = v1 then Past.Integer(l, x) else e1
            | Past.Integer(_, _) -> e1
            | _ -> raise (Err "Invalid cell composition")
          in Past.RC(l, f r, f c)  
        | Past.Corner(l, e) -> Past.Corner(l, loop e)
        | Past.Op(l, e1, op, e2) -> Past.Op(l, loop e1, op, loop e2)
        | Past.UnaryOp(l, uop, e1) -> Past.UnaryOp(l, uop, loop e1)
        | Past.SpecOp(l, e1, sop, e2) -> Past.SpecOp(l, loop e1, sop, loop e2)
        | Past.Dec(l, dt, e1, e2) -> (match e2 with
          | Some e3 -> Past.Dec(l, dt, loop e1, Some (loop e3))
          | None -> e)
        | Past.Assign(l, e1, e2) -> Past.Assign(l, loop e1, loop e2)
        | Past.Utils(l, e1, u) -> Past.Utils(l, loop e1, u)
        | Past.Quantifier(l, q, e1, g, e2) -> let Past.Group(_, ng) = loop (Past.Group(l, g)) in Past.Quantifier(l, q, loop e1, ng, loop e2)
        | Past.List(l, ls) -> Past.List(l, List.map loop ls)
        | Past.Group(l, g) -> (match g with
          | Past.Instance(e1) -> Past.Group(l, Past.Instance(loop e1))
          | _ -> e)
        | Past.Range(l, e1, e2) -> Past.Range(l, loop e1, loop e2)
        | Past.Member(l, e1, e2) -> Past.Member(l, loop e1, loop e2)
        | Past.Sugar(l, dt, e1, c) -> Past.Sugar(l, dt, loop e1, c)
        | Past.CellDec(l, e1) -> Past.CellDec(l, loop e1)
        | Past.ITE(l, e1, e2, e3) -> Past.ITE(l, loop e1, loop e2, loop e3)
        | _ -> e
    in loop e
  in let rec loop2 ls = function
    | (v, x)::vxs -> let loc = get_loc (List.hd ls) in 
      loop2 (List.concat_map (fun e -> List.map (fun y -> Past.Op(loc, Past.Op(loc, Past.Var(loc, v), Past.Equal,
        Past.Integer(loc, y)), Past.And, helper v y e)) (List.init x (Int.add 1))) ls) vxs
    | [] -> ls
  in loop2 [expr] vs

let scan_utils expr vars =
  let rec match_group = function
    | Past.Instance(e) -> loop e
    | _ -> []
  and loop e = 
    match e with
      | Past.RC(_, e1, e2) -> loop e1 @ loop e2
      | Past.Corner(_, e1) -> loop e1
      | Past.Op(_, e1, _, e2) -> loop e1 @ loop e2
      | Past.UnaryOp(_, _, e1) -> loop e1
      | Past.SpecOp(_, e1, _, e2) -> loop e1 @ loop e2
      | Past.Dec(_, _, e1, e2) -> loop e1 @ (match e2 with
        | Some e3 -> loop e3
        | None -> [])
      | Past.Assign(_, e1, e2) -> loop e1 @ loop e2
      | Past.Utils(_, e1, u) -> [e1]
      | Past.Quantifier(_, _, e1, g, e2) -> loop e1 @ loop e2 @ match_group g
      | Past.List(_, l) -> let f a b = loop a @ b in List.fold_right f l []
      | Past.Group(_, g) -> match_group g
      | Past.Range(_, e1, e2) -> loop e1 @ loop e2
      | Past.Member(_, e1, e2) -> loop e1 @ loop e2
      | Past.Sugar(_, _, e1, _) -> loop e1
      | Past.CellDec(_, e1) -> loop e1
      | Past.ITE(_, e1, e2, e3) -> loop e1 @ loop e2 @ loop e3
      | _ -> []
  in List.map get_var (loop expr)

let rec replace_utils expr vs = 
  let cells = get_cells 0
  in let loc = get_loc expr
  in let rec loop2 ls = function
    | var::vars -> List.concat_map (fun e -> List.map (fun (r, c) -> Past.Op(loc, Past.Var(loc, sprintf "%s_r%ic%i" var r c), Past.LeftImp,
      substitute (Past.RC(loc, Past.Integer(loc, r), Past.Integer(loc, c))) var e)) cells) ls
    | [] -> []
  in Past.List(loc, loop2 [expr] vs)
 
let rec store_vars dt vars = function
  | v::vs -> store_vars dt ((dt, v, None)::vars) vs
  | [] -> vars

let translate_rc r c vars = 
  let f e = match e with
    | Past.Integer(_, i) -> i
    | Past.Var(_) -> let (_, e1) = safe_find e vars in
      (match e1 with
        | Some (Past.Integer(_, i)) -> i
        | None -> raise (Err "RC Error"))
  in (Ast.Var(sprintf "r%ic%i" (f r) (f c)), vars)

let rec translate_term e1 op e2 vars =
  let normal = let (expr1, vars1) = translate_expr e1 vars in
    let (expr2, vars2) = translate_expr e2 vars1 in
    (Ast.Op(expr1, translate_op op, expr2), vars2)
  in match op with
    | Past.RightImp -> translate_term e2 Past.LeftImp e1 vars
    | Past.BiImp -> translate_term e1 Past.Equal e2 vars
    | Past.Equal -> (try let (dt, _) = safe_find e2 vars 
      in (match dt with
        | Past.Region -> translate_assignment e1 e2 vars
        | Past.CentreLine -> translate_assignment e1 e2 vars
        | _ -> normal)
      with NotVar -> normal)
    | _ -> normal

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
  match sop1 with
    | Past.RegionAdjacent -> let Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)) = e1 in 
      let Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)) = e2 in
      ((if (abs (r1-r2) + abs (c1-c2)) = 1 then 
      Ast.Op(Ast.Var(sprintf "%s_root" v1), Ast.Unequal, Ast.Var(sprintf "%s_root" v2)) else Ast.Dead), vars2)
    | Past.CellAdjacent -> let Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)) = e1 in 
      let Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)) = e2 in
      ((if (abs (r1-r2) + abs(c1-c2)) = 1 then Ast.Boolean(true) else Ast.Boolean(false)), vars2)
    | Past.LineAdjacent(v) -> 
      let Past.RC(_, Past.Integer(_, r1), Past.Integer(_, c1)) = e1 in 
      let Past.RC(_, Past.Integer(_, r2), Past.Integer(_, c2)) = e2 in
      ((if abs (r1-r2) <= 1 && abs (c1-c2) <= 1 then
      let (_, Some (Past.List(_, ls))) = safe_find v vars 
      in let rec loop = function
        | Past.Range(_, Past.RC(_, Past.Integer(_, ra1), Past.Integer(_, ca1)), 
          Past.RC(_, Past.Integer(_, ra2), Past.Integer(_, ca2)))::es -> 
          if (r1, c1, r2, c2) = (ra1, ca1, ra2, ca2) || (r1, c1, r2, c2) = (ra2, ca2, ra1, ca1)
          then Ast.Boolean(true) else loop es
        | [] -> Ast.Boolean(false)
      in loop ls else Ast.Dead), vars2)
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
    | Past.Int -> (match e with
      | Some Past.Integer(_, i) -> (Ast.Bundle([Ast.Dec(Ast.Int, nv); Ast.Op(nv, Ast.Equal, Ast.Integer(i))]), (Past.Int, v1, e)::vars)
      | None -> (Ast.Dec(Ast.Int, nv), (Past.Int, v1, None)::vars))
    | Past.Bool -> (match e with
      | Some Past.Boolean(_, b) -> (Ast.Bundle([Ast.Dec(Ast.Bool, nv); nv]), (Past.Bool, v1, e)::vars)
      | None -> (Ast.Dec(Ast.Bool, nv), (Past.Bool, v1, None)::vars))
    | Past.Cell -> (match e with 
      | None -> (Ast.Dead, (Past.Cell, v1, None)::vars))
    | Past.Region -> let init = [] in
      (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_region l init v1 vars
      | Some Past.Var(l, v2) -> let (dt, expr) = safe_find (Past.Var(l, v2)) vars 
        in (Ast.Dead, (dt, (Past.Var(l, v2)), expr)::vars)
      | None -> (Ast.Bundle(init), (Past.Region, v1, None)::vars))
    | Past.CentreLine -> let init = create_centreline nv in 
      (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_centreline l init v1 vars
      | None -> (Ast.Bundle(init), (Past.CentreLine, v1, None)::vars))
    | Past.EdgeLine -> let init = create_edgeline nv in
      (match e with
        | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_edgeline l init v1 vars
        | None -> (Ast.Bundle(init), (Past.EdgeLine, v1, None)::vars))
    | Past.CentreLoop -> let init = create_centreloop nv in
      (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_centreline l init v1 vars
      | None -> (Ast.Bundle(init), (Past.CentreLine, v1, None)::vars))
    | Past.Box -> (match e with
      | Some Past.Group(_, Past.Instance(Past.List(_, l))) -> define_box l v1 vars
      | _ -> raise (Err "box undefined"))
    | _ -> raise (Err "Unimplemented datatype")

  in let rec loop vars = function
    | v::vs -> let (ne, nvars) = helper v vars in (ne, nvars)::(loop nvars vs)
    | [v] -> [helper v vars]
    | [] -> []
  in match v with 
    | Past.List(_, l) -> let nl = loop vars l in
      let (es, vs) = List.split nl in (Ast.Bundle(es), List.nth vs ((List.length vs)-1))

and translate_assignment v e vars = 
  let (dt, _) = safe_find v vars 
  in match dt with
    | Past.Region -> let Past.List(_, l) = e in define_region l [] v vars
    | Past.CentreLine -> let Past.List(_, l) = e in 
      let (line, nvars) = translate_centreline l (get_var v) vars
      in (Ast.Bundle(line), (Past.CentreLine, v, Some nvars)::vars)

and translate_utils e u vars =
  let v = get_var e
  in match u with
  | Past.Size -> (match e with
    | Past.Utils(_, e1, Past.Reg) -> translate_utils e1 u vars
    | _ -> (Ast.Var(sprintf "%s_size" v), vars))
  | Past.Reg -> (Ast.Var(sprintf "%s_root" v), vars)
  | Past.Sum -> (Ast.Var(sprintf "%s_sum" v), vars)
    
and translate_quantifier l q d g c vars =
  match q with
    | Past.NForAll -> translate_expr (Past.UnaryOp(l, Past.Not, Past.Quantifier(l, q, d, g, c))) vars
    | Past.NExists -> translate_expr (Past.UnaryOp(l, Past.Not, Past.Quantifier(l, q, d, g, c))) vars
    | _ -> let op = match q with
        | Past.ForAll -> Ast.And
        | Past.Exists -> Ast.Or
      in let split group ng ls = match ls with
        | [vx] -> Ast.MultiOp(op, List.map (fun rc -> let (expr, _) = 
          translate_expr (substitute rc (get_var vx) c) vars in expr) group)
        | vx::vs ->
          let Past.Dec(_, dt, _, _) = d in
          let (nv, _) = translate_quantifier l q (Past.Dec(l, Past.Cell, Past.List(l, vs), None)) ng
            (Past.Quantifier(l, q, (Past.Dec(l, Past.Cell, Past.List(l, [vx]), None)), ng,
            replace_spec_op c (store_vars dt vars ls))) vars
          in nv
      in match d with
        | Past.Dec(_, Past.Cell, Past.List(_, ls), _) -> 
          (match g with
            | Past.Row(Some (Past.Var(_, v))) -> (Ast.MultiOp(Or, (List.mapi (fun i -> fun group -> 
                Ast.Op(Ast.Op(Ast.Var(v), Ast.Equal, Ast.Integer(!gridr-i)), Ast.And, split group (Past.Row(Some (Past.Integer(l, !gridr-i)))) ls)) 
                (List.map (fun lss -> List.map (fun (r, c) -> Past.RC(l, Past.Integer(l, r), Past.Integer(l, c))) lss) (rows ())))), vars)
            | _ -> (split (translate_group l g vars) g ls, vars))
        | Past.Dec(_, Past.Region, Past.List(_, ls), _) ->
          let (reg, _) = translate_expr d vars 
          in (Ast.Bundle(reg::[split (cell_grid l) g ls]), vars)

and translate_group l g vars = 
  let to_rc (r, c) = Past.RC(l, Past.Integer(l, r), Past.Integer(l, c))
  in match g with
    | Past.Grid -> cell_grid l
    | Past.Row(Some e) -> (match e with
      | Past.Integer(_, i) -> List.map to_rc (row i)
      | Past.Var(_) -> let (_, e1) = safe_find e vars 
        in match e1 with
        | Some (Past.Integer(_, i)) -> List.map to_rc (row i))
    | Past.Instance(e) -> (match e with
      | Past.List(_, es) -> (expand_list es vars)
      | Past.Var(_) -> let (dt, Some ls) = safe_find e vars 
        in match dt, ls with
          | Past.Box, Past.List(_, list) -> list
          | Past.Region, Past.List(_, list) -> list
          | Past.CentreLine, Past.List(_, list) -> List.map to_rc (get_centreline_cells list))
        
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
      | Past.Grid -> (Ast.MultiOp(con, to_var (get_cells 0)), vars)
      | Past.Row(None) -> (Ast.MultiOp(Ast.And, List.map (fun l -> Ast.MultiOp(con, to_var l)) (rows ())), vars)
      | Past.Column(None) -> (Ast.MultiOp(Ast.And, List.map (fun l -> Ast.MultiOp(con, to_var l)) (columns ())), vars)
      | Past.Boxes(None) -> (Ast.MultiOp(Ast.And, (List.filter_map (fun (dt, _, Some e) ->
        match e with
          | Past.List(_, ls) -> 
            if dt = Past.Box then Some (Ast.MultiOp(con, List.map (fun (Past.RC(_, Past.Integer(_, r), Past.Integer(_, c))) -> 
            Ast.Var(sprintf "r%ic%i" r c)) ls)) else None
          | _ -> None) vars)), vars)
      | Past.Regions -> (Ast.MultiOp(Ast.And, (List.filter_map (fun (dt, _, Some e) ->
        match e with
          | Past.List(_, ls) -> if dt = Past.Region then 
              Some (Ast.MultiOp(con, List.map (fun (Past.RC(_, Past.Integer(_, r), Past.Integer(_, c))) -> 
              Ast.Var(sprintf "r%ic%i" r c)) ls)) else None
          | _ -> None) vars)), vars)
      | Past.Instance(v) ->  (
        
        let (_, Some e) = safe_find v vars in 
        match e with
          | Past.List(_, ls) -> Ast.MultiOp(Ast.Unequal, List.map 
            (fun (Past.RC(_, Past.Integer(_, r), Past.Integer(_, c))) -> Ast.Var(sprintf "r%ic%i" r c)) ls), vars))

and translate_celldec e vars =
  match e with 
    | Past.Range(_, Past.Integer(_, i1), Past.Integer(_, i2)) -> 
      (Ast.Bundle(List.map (fun (r, c) -> Ast.Op(Ast.Op(Ast.Integer(i1), Ast.LTE, Ast.Var(sprintf "r%ic%i" r c)), 
      Ast.And, Ast.Op(Ast.Var(sprintf "r%ic%i" r c), Ast.LTE, Ast.Integer(i2)))) (get_cells 0)), vars)

and translate_ite e1 e2 e3 vars = 
  let (expr1, _) = translate_expr e1 vars in
  let (expr2, _) = translate_expr e2 vars in
  let (expr3, _) = translate_expr e3 vars in
  (Ast.ITE(expr1, expr2, expr3), vars)

and translate_expr e vars = 
  match e with
  | Past.Integer(_, n) -> (Ast.Integer(n), vars)
  | Past.Boolean(_, b) -> (Ast.Boolean(b), vars)
  | Past.RC(_, r, c) -> translate_rc r c vars
  | Past.Var(_, v) -> (Ast.Var(v), vars)
  | Past.Op(_, e1, op, e2) -> translate_term e1 op e2 vars
  | Past.UnaryOp(_, uop, e) -> translate_unary_term uop e vars
  | Past.SpecOp(l, e1, sop, e2) -> translate_spec_term l e1 sop e2 vars
  | Past.Dec(_, d, v, e) -> translate_dec d v e vars
  | Past.Assign(_, v, e) -> translate_assignment v e vars
  | Past.Utils(_, e, u) -> translate_utils e u vars
  | Past.Quantifier(l, q, d, g, c) -> translate_quantifier l q d g c vars
  | Past.List(_, l) -> translate_list l vars
  | Past.Member(_, e1, e2) -> translate_member e1 e2 vars
  | Past.Sugar(_, dt, e, c) -> translate_sugar dt e c vars 
  | Past.CellDec(_, e) -> translate_celldec e vars
  | Past.ITE(_, e1, e2, e3) -> translate_ite e1 e2 e3 vars
  | Past.Group(_) -> raise (Err "Unexpected group")

let init_named_regions vars =
  let rec unpack ls = match ls with
    | e::es -> (get_rc e vars)::(unpack es)
    | [] -> []
  in let rec loop = function
    | (Past.Region, Past.Var(_, v), None)::es -> 
      (List.map (fun (r, c) -> Ast.Dec(Ast.Bool, Ast.Var(sprintf "%s_r%ic%i" v r c))) (get_cells 0))@(loop es)
    | (Past.Region, Past.Var(_, v), Some (Past.List(_, l)))::es -> 
      (List.map (fun (r, c) -> Ast.Dec(Ast.Bool, Ast.Var(sprintf "%s_r%ic%i" v r c))) (get_cells 0))
      @ (List.map (fun (r, c) -> Ast.Var(sprintf "%s_r%ic%i" v r c)) (unpack l))@(loop es)
    | _::es -> loop es
    | [] -> []
  in loop vars

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
    | [_] -> []
    | x::xs -> x::(loop xs)
  in (List.nth l ((List.length l)-1))::(loop l)

let rec init_vars l vars e uninit = 
  let rec loop vars = function
    | (v, x)::ls -> Ast.MultiOp(Ast.Or, List.map (fun y -> Ast.Op(Ast.Op(Ast.Var(v), Ast.Equal, Ast.Integer(y)), 
      Ast.And, loop ((Past.Int, Past.Var(l, v), Some (Past.Integer(l, y)))::vars) ls))
      (List.init x (Int.add 1)))
    | [] -> let (expr, _) = translate_expr e vars in expr
  in loop vars uninit 

let convert = function
  | ((_, r, c), xs) -> let _ = gridr := r; gridc := c; in
    let rec loop exprs vars = 
      match exprs with
      | e::es -> let uninit_vars = scan_rc e vars in
        let uninit_group = scan_utils e vars in
        (*
        let expr2 = replace_utils e uninit_group in   *)
        let expr = init_vars (get_loc e) vars e uninit_vars in
        let (_, nvars) = try translate_expr e vars with RegVar v -> (Ast.Dead, (Past.Region, v, None)::vars) in (*possible bug*)
        (expr, nvars)::loop es nvars
      | [] -> []
    in let translated = loop xs []
    in let (_, vars) = List.nth translated ((List.length translated)-1)
    in let find_reg vars =
      let rec loop = function
        | (Past.Region, _, _)::_ -> Ast.Bundle((init_regions ())@(init_named_regions vars))
        | _::vs -> loop vs
        | _ -> Ast.Dead
      in loop vars
    in ((r, c), flatten ((find_reg vars)::List.map (fun (e, v) -> e) translated))


