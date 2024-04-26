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
  | CENTRELOOP
  | EDGELOOP
  | R
  | C
  | INTDEC
  | BOOLDEC
  | ROW
  | COLUMN
  | BOX
  | CELLDEC
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
  | HIGH
  | LOW

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ((Lexing.position * int * int) * Past.expr list)
