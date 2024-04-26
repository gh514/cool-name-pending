

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
  | CentreLine
  | EdgeLine
  | CentreLoop
  | EdgeLoop
  | Box

type unary_op = 
  | Neg
  | Not
  | Abs

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

type spec_op =
  | Adjacent of expr option
  | CellAdjacent
  | RegionAdjacent
  | LineAdjacent of expr
  | CellLineAdjacent of int

and group = 
  | Grid
  | Row of expr option
  | Column of expr option
  | Regions
  | Boxes of expr option
  | Universe
  | Instance of expr

and expr = 
  | Integer of loc * int
  | Boolean of loc * bool
  | RC of loc * expr * expr
  | Corner of loc * expr
  | Var of loc * var
  | Op of loc * expr * op * expr
  | UnaryOp of loc * unary_op * expr
  | SpecOp of loc * expr * spec_op * expr
  | Dec of loc * data_type * expr * (expr option)
  | Assign of loc * expr * expr
  | Utils of loc * expr * utilities
  | Quantifier of loc * quant * expr * group * expr
  | List of loc * (expr list)
  | Group of loc * group
  | Range of loc * expr * expr
  | Member of loc * expr * expr
  | Sugar of loc * data_type * expr * constraints
  | CellDec of loc * expr

