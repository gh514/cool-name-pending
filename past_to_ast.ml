

let translate_var = function
  | Past.Var(_, v) -> Ast.Var(v)

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

let translate_unary_op = function
  | Past.Neg -> Ast.Neg
  | Past.Not -> Ast.Not

let rec translate_date_type = function
  | Past.Cell -> Ast.Cell
  | Past.Region -> Ast.Region
  | Past.Line -> Ast.Line
  | Past.Set(dt) -> Ast.Set(translate_date_type(dt))

let rec translate_seq = function
  | e::es -> (translate_expr e) :: (translate_seq es)
  | [] -> []


and translate_expr = function
  | Past.Integer(_, n) -> Ast.Integer(n)
  | Past.Boolean(_, b) -> Ast.Boolean(b)
  | Past.Var(_, v) -> Ast.Var(v)
  | Past.Op(_, e1, op, e2) -> Ast.Op(translate_expr e1, translate_op op, translate_expr e2)
  | Past.UnaryOp(_, uop, e) -> Ast.UnaryOp(translate_unary_op uop, translate_expr e)
  | Past.Seq(_, e) -> Ast.Seq(translate_seq e)
  | Past.Grid(_, r, c) -> Ast.Grid(r, c)
  | Past.Dec(_, dt, e) -> Ast.Dec(translate_date_type dt, translate_expr e)