type token =
  | INT of (int)
  | VAR of (string)
  | TRUE
  | FALSE
  | GRID
  | CROSS
  | CELL
  | REGION
  | CENTRELINE
  | EDGELINE
  | R
  | C
  | INTDEC
  | BOOLDEC
  | ROW
  | COLUMN
  | BOX
  | ADD
  | SUB
  | MUL
  | DIV
  | ABS
  | DIFF
  | AND
  | OR
  | NOT
  | XOR
  | EQUAL
  | LT
  | GT
  | LTE
  | GTE
  | UNEQUAL
  | MEMBER
  | ADJACENT
  | LEFTIMP
  | RIGHTIMP
  | BIIMP
  | LBRACK
  | RBRACK
  | LSBRACK
  | RSBRACK
  | SEMICOLON
  | POINT
  | TO
  | COMMA
  | HALF
  | FORALL
  | EXISTS
  | NFORALL
  | NEXISTS
  | IN
  | ARE
  | SIZE
  | LENGTH
  | SUM
  | DISTINCT
  | EQUIVALENT
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ((Lexing.position * int * int) * Past.expr list)
