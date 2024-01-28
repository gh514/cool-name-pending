type token =
  | INT of (int)
  | VAR of (string)
  | TRUE
  | FALSE
  | GRID
  | CELL
  | SET
  | REGION
  | LINE
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
  | FORALL
  | EXISTS
  | NFORALL
  | NEXISTS
  | IN
  | CELLS
  | VALUE
  | SIZE
  | LENGTH
  | ADJACENT
  | SPACE
  | EOF

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Past.expr
