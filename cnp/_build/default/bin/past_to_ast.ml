

include Printf

exception Err of string

let gridr = ref 2
let gridc = ref 1
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
  | _ -> raise (Err "Incorrect expression, Integer expected")

let get_bool = function
  | Past.Boolean(_, b) -> b
  | _ -> raise (Err "Incorrect expression, Boolean expected")

let get_var = function
  | Past.Var(_, v) -> v
  | _ -> raise (Err "Incorrect expression, Var expected")

let get_var_ast = function
  | Ast.Var(v) -> v
  | _ -> raise (Err "Incorrect expression, Var expected")

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
    Ast.Op(Ast.Var(sprintf "r%ic%i_num" r c), Ast.Equal,
      Ast.MultiOp(Ast.Add, Ast.Integer(1) ::
        (let rec loop = function
          | ((_, _), (ry, cy))::ls -> Ast.ITE(
            make_var_pair r c ry cy, Ast.Var(sprintf "r%ic%i_num" ry cy), Ast.Integer(0)) :: loop ls
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
    | ((r1, c1), (r2, c2)) :: ls -> Ast.Op(make_var_pair r1 c1 r2 c2, Ast.LeftImp, 
      Ast.Op(Ast.Var(sprintf "r%ic%i_root" r1 c1), Ast.Equal, Ast.Var(sprintf "r%ic%i_root" r2 c2)))::
      Ast.Op(make_var_pair r2 c2 r1 c1, Ast.LeftImp, 
      Ast.Op(Ast.Var(sprintf "r%ic%i_root" r2 c2), Ast.Equal, Ast.Var(sprintf "r%ic%i_root" r1 c1)))::(loop ls)
    | [] -> []
  in loop f


let get_root_constraints () =
  let m = !gridr in
  let n = !gridc in

  let root_constraints r c =
    Ast.Op(Ast.UnaryOp(Ast.Not, Ast.MultiOp(Ast.Or, 
      List.map (fun ((_, _), (rx, cx)) -> make_var_pair rx cx r c) (adj r c))), Ast.LeftImp,
      Ast.Op(Ast.Op(Ast.Var(sprintf "r%ic%i_size" r c), Ast.Equal, Ast.Var(sprintf "r%ic%i_num" r c)),
        Ast.And, Ast.Op(Ast.Var(sprintf "r%ic%i_root" r c), Ast.Equal, Ast.Integer((r-1)*n + c)))
      )

  in let rec root_loop r c =
    match (r, c) with
    | 0, _ -> []
    | _, 0 -> root_loop (r-1) n
    | _, _ -> (root_constraints r c) :: root_loop r (c-1)

  in root_loop m n




  (*
let parent_constraints f = 
  let rec loop = function
    | v::ls -> Ast.Op(Ast.Var(sprintf "%s_num" (String.sub (get_var v) 0 4))
      Ast.ITE(v, Ast.Op(Ast.Var(sprintf "%s_" (String.sub (get_var v) 0 4)), ))

  in loop f 
*)
(*
let size_constraints f =
  let rec loop = function
    | c::ls -> Ast.Op(c, Ast.LeftImp, Ast.Op(Ast.Var()))
*)

let init_regions _ =
  let grid = create_vars () in
  let field = List.map (fun v -> Ast.Dec(Ast.Bool, v)) (unzip (get_vars grid)) in
  let constr_field = direction_constraints (get_vars grid) in
  let size_grid = List.map (fun v -> Ast.Dec(Ast.Int, v)) (int_grid "size") in
  let num_grid = List.map (fun v -> Ast.Dec(Ast.Int, v)) (int_grid "num") in
  let parent_constr = get_parent_constraints () in
  let children_constr = get_children_constraints () in
  let size_constr = size_constraints grid in
  let root_constr = get_root_constraints () in
    Ast.Bundle(field @ size_grid @ num_grid @ constr_field @ parent_constr @ children_constr @ size_constr @ root_constr)

let translate_list xs = 
  let rec loop = function
    | (Past.RC(l, r, c))::es -> 
      Past.Var(l, sprintf "r%ic%i" (get_int r) (get_int c)) :: loop es
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

and translate_dec d e v vars = 
  let (ne, _) = translate_expr e vars in
  match d with
  | Past.Int -> (Ast.Dec(Ast.Int, ne), (Ast.Int, ne, None)::vars)
  | Past.Cell -> (Ast.Dead, (Ast.Cell, ne, None)::vars)
  | Past.Region -> if !regions then (Ast.Dead, (Ast.Region, ne, None)::vars)
    else (regions:=true; (init_regions (), (Ast.Region, ne, None)::vars))
    
    (*let grid = bool_grid (get_var e) in 
    let nlist = match v with 
      | Some(Past.Group(_, Past.Instance(Past.List(_, list)))) -> List.map (fun rc -> 
        let (expr, _) = translate_expr rc vars in expr) list in
        (Ast.Dec(Ast.Region, Ast.Group(ne, grid)) ,(Past.Region, ne, Some grid)::vars)
  
    (Ast.Bundle(
      Ast.Dec(Ast.Region, Ast.Group(ne, grid)) ::
        List.map (fun x -> (List.map (fun c -> if Str.string_match (Str.regexp (get_var_ast c)) x 0 then c else Ast.UnaryOp(Ast.Neg, c)
          
          ) grid)) nlist
    )

    *)
    
  | _ -> raise (Err "Unimplemented datatype")

  (*
  | Past.Line -> let ex = translate_expr e vars in
    match ex with 
    | (v, vs) -> let field = bool_field !gridc !gridr in  
      (Ast.Group(v, field), (v, Some field) :: vs)
    | _ -> raise (Err "Line declaration error")
*)

and translate_group l g = match g with
  | Past.Grid -> cell_grid l
  | Past.Instance(e) ->
    (match e with
      | Past.List(_, es) -> translate_list es)
    
and translate_quantifier l q d g c vars =
  let op = match q with
    | Past.ForAll -> Ast.And
    | Past.Exists -> Ast.Or
  in match d with
    | Past.Dec(_, Past.Cell, var, _) -> 
      let (_, nvars) = translate_expr var vars in
      (Ast.MultiOp(op, (List.map (fun v -> let (expr, _) = 
        translate_expr (substitute (get_var v) (get_var var) c) vars in expr) (translate_group l g))), nvars)

    | Past.Dec(_, Past.Region, r, _) -> 
      let (reg, nvars) = translate_expr d vars in
      (Ast.Bundle([reg; Ast.MultiOp(op, List.map (fun v -> let (expr, _) = 
        translate_expr (substitute (get_var v) (get_var r) c) vars in expr) (cell_grid l))]), nvars)

and translate_region_term e1 rop e2 vars =
  let (Ast.Var(v1), vars1) = translate_expr e1 vars in
  let (Ast.Var(v2), vars2) = translate_expr e2 vars1 in
  let rc1 = Ast.Var(sprintf "%sTo%s" v1 v2) in
  let rc2 = Ast.Var(sprintf "%sTo%s" v2 v1) in

  if List.mem (rc1, rc2) (get_vars (create_vars ())) or List.mem (rc2, rc1) (get_vars (create_vars ())) then 
    match rop with
    | Past.Adjacent -> (Ast.UnaryOp(Ast.Not, Ast.Op(rc1, Ast.Or, rc2)), vars2)
  else (Ast.Dead, vars2)

and translate_utils e u vars =
  match u with
  | Past.Size -> (Ast.Var(sprintf "%s_size" (get_var e)), vars)
  

 
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
  | Past.Dec(_, d, e, v) -> translate_dec d e v vars
  | Past.Quantifier(l, q, d, g, c) -> translate_quantifier l q d g c vars
  | Past.Utils(_, e, u) -> translate_utils e u vars

let rec flatten = function
  | Ast.Bundle(l)::es -> (flatten l) @ flatten es
  | Ast.Dead::es -> flatten es
  | e::es -> e::(flatten es)
  | [] -> []

let clean e =
  let rec loop = function
    | Ast.Dead::ls -> loop ls
    
    | ex::ls -> ex::(loop ls)
    | [] -> []
  in loop e

let convert = function
  | ((_, r, c), xs) -> gridr := r; gridc := c;
    let rec loop exprs vars = 
      match exprs with
      | e::es -> let (expr, nvars) = translate_expr e vars in
        expr :: (loop es nvars)
      | [] -> []
    in ((r, c), flatten (loop xs []))
