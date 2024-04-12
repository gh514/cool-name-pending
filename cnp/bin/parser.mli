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
  | R
  | C
  | INTDEC
  | BOOLDEC
  | ROW
  | COLUMN
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
  | MEMBER
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
  | FORALL
  | EXISTS
  | NFORALL
  | NEXISTS
  | IN
  | ARE
  | SIZE
  | LENGTH
  | SUM
  | ADJACENT
  | DISTINCT
  | EQUIVALENT
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> ((Lexing.position * int * int) * Past.expr list)
