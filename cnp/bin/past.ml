

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
  | Box

type unary_op = 
  | Neg
  | Not
  | Abs

type region_op =
  | Adjacent

type utilities =
  | Cells
  | Value
  | Size
  | Length
  | Reg
  | Sum

type quant =
  | ForAll
  | Exists
  | NForAll
  | NExists

type constraints =
  | Distinct
  | Equivalent

type group = 
  | Grid
  | Row
  | Column
  | Regions
  | Boxes
  | Universe
  | Instance of expr

and expr = 
  | Integer of loc * int
  | Boolean of loc * bool
  | RC of loc * expr * expr
  | Var of loc * var
  | Op of loc * expr * op * expr
  | UnaryOp of loc * unary_op * expr
  | RegionOp of loc * expr * region_op * expr
  | Dec of loc * data_type * expr * (expr option)
  | Assign of loc * expr * expr
  | Utils of loc * expr * utilities
  | Quantifier of loc * quant * expr * group * expr
  | List of loc * (expr list)
  | Group of loc * group
  | Range of loc * expr * expr
  | Member of loc * expr * expr
  | Sugar of loc * data_type * expr * constraints

