type token =
  | INT of (int)
  | VAR of (string)
  | TRUE
  | FALSE
  | GRID
  | CROSS
  | CELL
  | REGION
  | LINE
  | ROW
  | COLUMN
  | INTDEC
  | BOOLDEC
  | ADD
  | SUB
  | MUL
  | DIV
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
  | LEFTIMP
  | RIGHTIMP
  | BIIMP
  | LBRACK
  | RBRACK
  | LSBRACK
  | RSBRACK
  | SEMICOLON
  | COMMA
  | POINT
  | TO
  | FORALL
  | EXISTS
  | NFORALL
  | NEXISTS
  | IN
  | CELLS
  | SIZE
  | LENGTH
  | SUM
  | ADJACENT
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ((Lexing.position * int * int) * Past.expr list)
