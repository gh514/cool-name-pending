

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

type expr = 
  | Integer of loc * int
  | Boolean of loc * bool
  | Op of loc * expr * op * expr
  | Seq of loc * (expr list)
  | Grid of loc
  | Dec of loc * data_type * expr
  | Utils of loc * expr * utilities
  | Quantifier of loc * quant * expr * expr * expr

type quant =
  | ForAll
  | Exists

type group = 
  | Grid
  | Universe
  | Instance of data_type

type utilities =
  | Cells
  | Value
  | Size
  | Length

  