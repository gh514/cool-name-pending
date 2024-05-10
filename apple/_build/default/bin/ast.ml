

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

type unary_op = 
  | Neg
  | Not
  | Abs

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
  | MultiOp of op * (expr list)
  | Dec of data_type * expr
  | Bundle of expr list
  | ITE of expr * expr * expr
  | Dead