

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
  | Cell
  | Region
  | Line
  | Set of data_type

type unary_op = 
  | Neg
  | Not

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
  | Instance of data_type

type expr = 
  | Integer of int
  | Boolean of bool
  | RC of expr * expr
  | Var of var
  | Op of expr * op * expr
  | UnaryOp of unary_op * expr
  | Seq of (expr list)
  | Grid of int * int
  | Dec of data_type * expr
  | Utils of expr * utilities
  | Quantifier of quant * expr * expr * expr
  