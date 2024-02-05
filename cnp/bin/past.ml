

type var = string

type loc = Lexing.position

type op = 
  | Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Xor
  | Equal
  | LT
  | GT
  | LTE
  | GTE
  | Unequal
  | LeftImp
  | RightImp
  | BiImp

type data_type = 
  | Int
  | Bool
  | Cell
  | Region
  | Line
  | Set of data_type

type unary_op = 
  | Neg
  | Not

type region_op =
  | Adjacent

type utilities =
  | Cells
  | Value
  | Size
  | Length

type quant =
  | ForAll
  | Exists

type group = 
  | Grid
  | Universe
  | Instance of expr

and expr = 
  | Integer of loc * int
  | Boolean of loc * bool
  | RC of loc * expr * expr
  | Var of loc * var
  | Op of loc * expr * op * expr
  | UnaryOp of loc * unary_op * expr
  | RegionOp of loc * region_op * expr
  | Seq of loc * (expr list)
  | Dec of loc * data_type * expr * (expr option)
  | Utils of loc * expr * utilities
  | Quantifier of loc * quant * expr * group * expr
  | Assign of loc * expr * expr
  | List of loc * (expr list)
  | Group of loc * group

(*Line l = [R1C1 to R2C2, R2C2 to R2C3]*)


  