

type var = string

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

type data_type = 
  | Int
  | Bool
  | Cell
  | Region
  | Line
  | Set of data_type

type multi_op =
  | MultiAnd 
  | MultiOr

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
  | Var of var
  | Op of expr * op * expr
  | UnaryOp of unary_op * expr
  | MultiOp of multi_op * (expr list)
  | Dec of data_type * expr
  | Utils of expr * utilities
  | Group of expr list
  | Bundle of expr list
  | Dead
  | ITE of expr * expr * expr