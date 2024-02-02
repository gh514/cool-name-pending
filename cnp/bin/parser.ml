type token =
  | INT of (int)
  | VAR of (string)
  | TRUE
  | FALSE
  | GRID
  | CROSS
  | CELL
  | SET
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
  | EOF

open Parsing;;
let _ = parse_error;;
# 3 "parser.mly"

let location = Parsing.symbol_start_pos;;

(*
    | FORALL dec POINT expr                
                                            {Past.Quantifier(location(), Past.ForAll, $2, $4, Past.Group(Past.Universe))}
    | EXISTS dec POINT expr   
                                            {Past.Quantifier(location(), Past.Exists, $2, $4, Past.Group(Past.Universe))}

*)

# 67 "parser.ml"
let yytransl_const = [|
  259 (* TRUE *);
  260 (* FALSE *);
  261 (* GRID *);
  262 (* CROSS *);
  263 (* CELL *);
  264 (* SET *);
  265 (* REGION *);
  266 (* LINE *);
  267 (* ROW *);
  268 (* COLUMN *);
  269 (* INTDEC *);
  270 (* BOOLDEC *);
  271 (* ADD *);
  272 (* SUB *);
  273 (* MUL *);
  274 (* DIV *);
  275 (* AND *);
  276 (* OR *);
  277 (* NOT *);
  278 (* XOR *);
  279 (* EQUAL *);
  280 (* LT *);
  281 (* GT *);
  282 (* LTE *);
  283 (* GTE *);
  284 (* UNEQUAL *);
  285 (* LEFTIMP *);
  286 (* RIGHTIMP *);
  287 (* BIIMP *);
  288 (* LBRACK *);
  289 (* RBRACK *);
  290 (* LSBRACK *);
  291 (* RSBRACK *);
  292 (* SEMICOLON *);
  293 (* COMMA *);
  294 (* POINT *);
  295 (* FORALL *);
  296 (* EXISTS *);
  297 (* NFORALL *);
  298 (* NEXISTS *);
  299 (* IN *);
  300 (* CELLS *);
  301 (* VALUE *);
  302 (* SIZE *);
  303 (* LENGTH *);
  304 (* ADJACENT *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* INT *);
  258 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\002\000\002\000\002\000\002\000\
\008\000\008\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\008\000\008\000\008\000\007\000\007\000\009\000\009\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\003\000\003\000\006\000\006\000\004\000\004\000\004\000\
\005\000\005\000\005\000\005\000\005\000\005\000\000\000"

let yylen = "\002\000\
\003\000\002\000\001\000\001\000\001\000\001\000\004\000\001\000\
\002\000\004\000\002\000\004\000\002\000\004\000\002\000\004\000\
\002\000\004\000\003\000\005\000\001\000\003\000\001\000\003\000\
\003\000\003\000\003\000\003\000\001\000\003\000\001\000\003\000\
\003\000\003\000\003\000\002\000\003\000\003\000\003\000\002\000\
\003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\008\000\008\000\004\000\002\000\001\000\002\000\003\000\
\001\000\001\000\001\000\001\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\063\000\000\000\000\000\005\000\006\000\
\003\000\004\000\021\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\053\000\000\000\
\000\000\002\000\000\000\000\000\000\000\008\000\031\000\000\000\
\000\000\059\000\000\000\060\000\061\000\057\000\058\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\001\000\052\000\
\000\000\062\000\000\000\000\000\000\000\000\000\000\000\000\000\
\030\000\000\000\022\000\000\000\000\000\025\000\026\000\027\000\
\028\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\041\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\056\000\014\000\000\000\016\000\018\000\007\000\010\000\
\012\000\024\000\000\000\000\000\020\000\000\000\000\000\000\000\
\000\000\000\000\000\000\050\000\051\000"

let yydgoto = "\002\000\
\004\000\027\000\028\000\029\000\040\000\005\000\030\000\031\000\
\050\000"

let yysindex = "\003\000\
\005\255\000\000\029\255\000\000\148\000\025\255\000\000\000\000\
\000\000\000\000\000\000\004\255\078\255\004\255\004\255\004\255\
\004\255\004\255\093\255\093\255\093\255\004\255\000\000\108\255\
\108\255\000\000\003\255\127\255\043\000\000\000\000\000\043\255\
\041\255\000\000\078\255\000\000\000\000\000\000\000\000\004\255\
\042\255\045\255\057\255\048\255\049\255\050\255\017\255\153\255\
\037\255\055\255\033\255\034\255\237\254\093\255\093\255\093\255\
\093\255\093\255\093\255\093\255\093\255\093\255\093\255\093\255\
\093\255\093\255\093\255\093\255\093\255\093\255\000\000\000\000\
\004\255\000\000\060\255\004\255\004\255\004\255\004\255\004\255\
\000\000\004\255\000\000\251\254\251\254\000\000\000\000\000\000\
\000\000\050\255\050\255\047\255\047\255\050\255\047\255\047\255\
\000\000\017\255\017\255\017\255\017\255\017\255\241\255\241\255\
\219\255\000\000\000\000\004\255\000\000\000\000\000\000\000\000\
\000\000\000\000\061\255\067\255\000\000\076\255\087\255\093\255\
\093\255\175\255\197\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\174\000\093\000\000\000\000\000\000\000\000\000\
\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\023\000\045\000\000\000\067\000\089\000\196\000\112\001\000\000\
\085\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\123\000\000\000\000\000\
\000\000\000\000\111\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\218\000\240\000\033\001\055\001\006\001\077\001\099\001\
\000\000\134\001\148\001\163\001\176\001\195\001\137\000\121\001\
\141\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\002\000\248\255\054\000\091\000\000\000\174\255\253\255\
\046\000"

let yytablesize = 743
let yytable = "\011\000\
\013\000\115\000\116\000\001\000\007\000\008\000\009\000\010\000\
\011\000\003\000\046\000\047\000\048\000\033\000\016\000\041\000\
\042\000\043\000\044\000\045\000\051\000\052\000\015\000\049\000\
\086\000\087\000\088\000\089\000\022\000\006\000\032\000\054\000\
\055\000\056\000\057\000\058\000\059\000\022\000\060\000\061\000\
\053\000\075\000\071\000\072\000\017\000\090\000\091\000\092\000\
\093\000\094\000\095\000\096\000\097\000\098\000\099\000\100\000\
\101\000\102\000\103\000\104\000\105\000\054\000\055\000\073\000\
\076\000\058\000\009\000\077\000\078\000\061\000\079\000\080\000\
\061\000\082\000\107\000\084\000\085\000\109\000\110\000\111\000\
\112\000\113\000\108\000\049\000\034\000\035\000\036\000\037\000\
\011\000\083\000\038\000\039\000\054\000\007\000\008\000\009\000\
\010\000\011\000\118\000\012\000\013\000\014\000\015\000\016\000\
\119\000\017\000\018\000\120\000\019\000\117\000\019\000\122\000\
\123\000\020\000\012\000\013\000\014\000\015\000\121\000\023\000\
\017\000\018\000\055\000\106\000\021\000\074\000\022\000\114\000\
\000\000\000\000\000\000\024\000\025\000\000\000\000\000\000\000\
\047\000\000\000\000\000\000\000\049\000\054\000\055\000\056\000\
\057\000\058\000\059\000\026\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\000\000\000\000\
\000\000\000\000\070\000\000\000\000\000\000\000\000\000\054\000\
\055\000\056\000\057\000\058\000\059\000\029\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\000\000\081\000\000\000\000\000\000\000\054\000\055\000\056\000\
\057\000\058\000\059\000\036\000\060\000\061\000\062\000\063\000\
\064\000\065\000\066\000\067\000\068\000\069\000\000\000\124\000\
\000\000\000\000\000\000\054\000\055\000\056\000\057\000\058\000\
\059\000\032\000\060\000\061\000\062\000\063\000\064\000\065\000\
\066\000\067\000\068\000\069\000\000\000\125\000\000\000\000\000\
\000\000\054\000\055\000\056\000\057\000\058\000\059\000\033\000\
\060\000\061\000\062\000\063\000\064\000\065\000\066\000\067\000\
\068\000\000\000\000\000\000\000\000\000\000\000\000\000\054\000\
\055\000\056\000\057\000\058\000\059\000\037\000\060\000\061\000\
\062\000\063\000\064\000\065\000\066\000\000\000\000\000\013\000\
\013\000\013\000\013\000\013\000\013\000\000\000\013\000\000\000\
\013\000\013\000\013\000\013\000\013\000\013\000\013\000\013\000\
\034\000\013\000\000\000\000\000\013\000\015\000\015\000\015\000\
\015\000\015\000\015\000\013\000\015\000\000\000\015\000\015\000\
\015\000\015\000\015\000\015\000\015\000\015\000\035\000\015\000\
\000\000\000\000\015\000\017\000\017\000\017\000\017\000\017\000\
\017\000\015\000\017\000\000\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\017\000\038\000\017\000\000\000\000\000\
\017\000\009\000\009\000\009\000\009\000\009\000\009\000\017\000\
\009\000\000\000\009\000\009\000\009\000\009\000\009\000\009\000\
\009\000\009\000\039\000\009\000\000\000\000\000\009\000\011\000\
\011\000\011\000\011\000\011\000\011\000\009\000\011\000\040\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\048\000\011\000\000\000\000\000\011\000\019\000\019\000\019\000\
\019\000\019\000\019\000\011\000\019\000\042\000\019\000\019\000\
\019\000\019\000\019\000\019\000\019\000\019\000\000\000\019\000\
\000\000\000\000\019\000\043\000\007\000\008\000\009\000\010\000\
\011\000\019\000\012\000\013\000\014\000\015\000\016\000\000\000\
\017\000\018\000\044\000\019\000\000\000\047\000\047\000\047\000\
\020\000\047\000\000\000\049\000\047\000\049\000\000\000\045\000\
\049\000\000\000\000\000\021\000\000\000\022\000\000\000\023\000\
\000\000\000\000\024\000\025\000\029\000\029\000\029\000\029\000\
\029\000\029\000\046\000\029\000\029\000\029\000\029\000\029\000\
\029\000\029\000\029\000\029\000\029\000\000\000\029\000\000\000\
\000\000\029\000\036\000\036\000\036\000\036\000\036\000\036\000\
\000\000\036\000\000\000\036\000\036\000\036\000\036\000\036\000\
\036\000\036\000\036\000\000\000\036\000\000\000\000\000\036\000\
\032\000\032\000\032\000\032\000\032\000\032\000\000\000\032\000\
\000\000\032\000\032\000\032\000\032\000\032\000\032\000\032\000\
\032\000\000\000\032\000\000\000\000\000\032\000\033\000\033\000\
\033\000\033\000\033\000\033\000\000\000\033\000\000\000\033\000\
\033\000\033\000\033\000\033\000\033\000\033\000\033\000\000\000\
\033\000\000\000\000\000\033\000\037\000\037\000\037\000\037\000\
\037\000\037\000\000\000\037\000\000\000\037\000\037\000\037\000\
\037\000\037\000\037\000\037\000\037\000\000\000\037\000\000\000\
\000\000\037\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\034\000\034\000\000\000\034\000\000\000\034\000\000\000\
\034\000\034\000\034\000\034\000\034\000\034\000\034\000\034\000\
\000\000\034\000\000\000\000\000\034\000\000\000\000\000\035\000\
\035\000\000\000\035\000\000\000\035\000\000\000\035\000\035\000\
\035\000\035\000\035\000\035\000\035\000\035\000\000\000\035\000\
\000\000\000\000\035\000\000\000\000\000\038\000\038\000\000\000\
\038\000\000\000\038\000\000\000\038\000\038\000\038\000\038\000\
\038\000\038\000\038\000\038\000\000\000\038\000\000\000\000\000\
\038\000\000\000\000\000\039\000\039\000\000\000\039\000\000\000\
\039\000\000\000\039\000\039\000\039\000\039\000\039\000\039\000\
\039\000\039\000\000\000\039\000\000\000\000\000\039\000\040\000\
\040\000\040\000\040\000\040\000\040\000\040\000\040\000\000\000\
\040\000\000\000\000\000\040\000\000\000\048\000\048\000\048\000\
\000\000\048\000\000\000\000\000\048\000\042\000\042\000\042\000\
\042\000\042\000\042\000\042\000\042\000\000\000\042\000\000\000\
\000\000\042\000\000\000\043\000\043\000\043\000\043\000\043\000\
\043\000\043\000\043\000\000\000\043\000\000\000\000\000\043\000\
\000\000\000\000\044\000\044\000\044\000\044\000\044\000\044\000\
\044\000\044\000\000\000\044\000\000\000\000\000\044\000\045\000\
\045\000\045\000\045\000\045\000\045\000\045\000\045\000\000\000\
\045\000\000\000\000\000\045\000\000\000\000\000\000\000\000\000\
\000\000\000\000\046\000\046\000\046\000\046\000\046\000\046\000\
\046\000\046\000\000\000\046\000\000\000\000\000\046\000"

let yycheck = "\005\001\
\000\000\084\000\085\000\001\000\001\001\002\001\003\001\004\001\
\005\001\005\001\019\000\020\000\021\000\012\000\011\001\014\000\
\015\000\016\000\017\000\018\000\024\000\025\000\000\000\022\000\
\044\001\045\001\046\001\047\001\034\001\001\001\006\001\015\001\
\016\001\017\001\018\001\019\001\020\001\034\001\022\001\023\001\
\038\001\040\000\000\000\001\001\000\000\054\000\055\000\056\000\
\057\000\058\000\059\000\060\000\061\000\062\000\063\000\064\000\
\065\000\066\000\067\000\068\000\069\000\015\001\016\001\023\001\
\023\001\019\001\000\000\023\001\012\001\023\001\023\001\023\001\
\023\001\037\001\073\000\043\001\043\001\076\000\077\000\078\000\
\079\000\080\000\023\001\082\000\007\001\008\001\009\001\010\001\
\000\000\035\001\013\001\014\001\000\000\001\001\002\001\003\001\
\004\001\005\001\038\001\007\001\008\001\009\001\010\001\011\001\
\038\001\013\001\014\001\032\001\016\001\108\000\000\000\120\000\
\121\000\021\001\007\001\008\001\009\001\010\001\032\001\035\001\
\013\001\014\001\000\000\070\000\032\001\035\000\034\001\082\000\
\255\255\255\255\255\255\039\001\040\001\255\255\255\255\255\255\
\000\000\255\255\255\255\255\255\000\000\015\001\016\001\017\001\
\018\001\019\001\020\001\000\000\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\255\255\255\255\
\255\255\255\255\036\001\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\000\000\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\255\255\015\001\016\001\017\001\
\018\001\019\001\020\001\000\000\022\001\023\001\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\255\255\033\001\
\255\255\255\255\255\255\015\001\016\001\017\001\018\001\019\001\
\020\001\000\000\022\001\023\001\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\255\255\033\001\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\019\001\020\001\000\000\
\022\001\023\001\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\255\255\255\255\255\255\255\255\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\000\000\022\001\023\001\
\024\001\025\001\026\001\027\001\028\001\255\255\255\255\015\001\
\016\001\017\001\018\001\019\001\020\001\255\255\022\001\255\255\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\000\000\033\001\255\255\255\255\036\001\015\001\016\001\017\001\
\018\001\019\001\020\001\043\001\022\001\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\000\000\033\001\
\255\255\255\255\036\001\015\001\016\001\017\001\018\001\019\001\
\020\001\043\001\022\001\255\255\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\000\000\033\001\255\255\255\255\
\036\001\015\001\016\001\017\001\018\001\019\001\020\001\043\001\
\022\001\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\000\000\033\001\255\255\255\255\036\001\015\001\
\016\001\017\001\018\001\019\001\020\001\043\001\022\001\000\000\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\000\000\033\001\255\255\255\255\036\001\015\001\016\001\017\001\
\018\001\019\001\020\001\043\001\022\001\000\000\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\255\255\033\001\
\255\255\255\255\036\001\000\000\001\001\002\001\003\001\004\001\
\005\001\043\001\007\001\008\001\009\001\010\001\011\001\255\255\
\013\001\014\001\000\000\016\001\255\255\029\001\030\001\031\001\
\021\001\033\001\255\255\031\001\036\001\033\001\255\255\000\000\
\036\001\255\255\255\255\032\001\255\255\034\001\255\255\036\001\
\255\255\255\255\039\001\040\001\015\001\016\001\017\001\018\001\
\019\001\020\001\000\000\022\001\023\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\255\255\033\001\255\255\
\255\255\036\001\015\001\016\001\017\001\018\001\019\001\020\001\
\255\255\022\001\255\255\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\255\255\033\001\255\255\255\255\036\001\
\015\001\016\001\017\001\018\001\019\001\020\001\255\255\022\001\
\255\255\024\001\025\001\026\001\027\001\028\001\029\001\030\001\
\031\001\255\255\033\001\255\255\255\255\036\001\015\001\016\001\
\017\001\018\001\019\001\020\001\255\255\022\001\255\255\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\255\255\
\033\001\255\255\255\255\036\001\015\001\016\001\017\001\018\001\
\019\001\020\001\255\255\022\001\255\255\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\255\255\033\001\255\255\
\255\255\036\001\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\017\001\018\001\255\255\020\001\255\255\022\001\255\255\
\024\001\025\001\026\001\027\001\028\001\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\036\001\255\255\255\255\017\001\
\018\001\255\255\020\001\255\255\022\001\255\255\024\001\025\001\
\026\001\027\001\028\001\029\001\030\001\031\001\255\255\033\001\
\255\255\255\255\036\001\255\255\255\255\017\001\018\001\255\255\
\020\001\255\255\022\001\255\255\024\001\025\001\026\001\027\001\
\028\001\029\001\030\001\031\001\255\255\033\001\255\255\255\255\
\036\001\255\255\255\255\017\001\018\001\255\255\020\001\255\255\
\022\001\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\255\255\033\001\255\255\255\255\036\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\255\255\
\033\001\255\255\255\255\036\001\255\255\029\001\030\001\031\001\
\255\255\033\001\255\255\255\255\036\001\024\001\025\001\026\001\
\027\001\028\001\029\001\030\001\031\001\255\255\033\001\255\255\
\255\255\036\001\255\255\024\001\025\001\026\001\027\001\028\001\
\029\001\030\001\031\001\255\255\033\001\255\255\255\255\036\001\
\255\255\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\255\255\033\001\255\255\255\255\036\001\024\001\
\025\001\026\001\027\001\028\001\029\001\030\001\031\001\255\255\
\033\001\255\255\255\255\036\001\255\255\255\255\255\255\255\255\
\255\255\255\255\024\001\025\001\026\001\027\001\028\001\029\001\
\030\001\031\001\255\255\033\001\255\255\255\255\036\001"

let yynames_const = "\
  TRUE\000\
  FALSE\000\
  GRID\000\
  CROSS\000\
  CELL\000\
  SET\000\
  REGION\000\
  LINE\000\
  ROW\000\
  COLUMN\000\
  INTDEC\000\
  BOOLDEC\000\
  ADD\000\
  SUB\000\
  MUL\000\
  DIV\000\
  AND\000\
  OR\000\
  NOT\000\
  XOR\000\
  EQUAL\000\
  LT\000\
  GT\000\
  LTE\000\
  GTE\000\
  UNEQUAL\000\
  LEFTIMP\000\
  RIGHTIMP\000\
  BIIMP\000\
  LBRACK\000\
  RBRACK\000\
  LSBRACK\000\
  RSBRACK\000\
  SEMICOLON\000\
  COMMA\000\
  POINT\000\
  FORALL\000\
  EXISTS\000\
  NFORALL\000\
  NEXISTS\000\
  IN\000\
  CELLS\000\
  VALUE\000\
  SIZE\000\
  LENGTH\000\
  ADJACENT\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'init) in
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr list) in
    Obj.repr(
# 48 "parser.mly"
                                            ((_1, _2))
# 459 "parser.ml"
               : ((Lexing.position * int * int) * Past.expr list)))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'init) in
    Obj.repr(
# 49 "parser.mly"
                                            ((_1, []))
# 466 "parser.ml"
               : ((Lexing.position * int * int) * Past.expr list)))
; (fun __caml_parser_env ->
    Obj.repr(
# 52 "parser.mly"
                                            (Past.Boolean(location(), true))
# 472 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 53 "parser.mly"
                                            (Past.Boolean(location(), false))
# 478 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 54 "parser.mly"
                                            (Past.Integer(location(), _1))
# 485 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 55 "parser.mly"
                                            (Past.Var(location(), _1))
# 492 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 56 "parser.mly"
                                            (Past.RC(location(), _2, _4))
# 500 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'group) in
    Obj.repr(
# 57 "parser.mly"
                                            (Past.Group(location(), _1))
# 507 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 60 "parser.mly"
                                                    (Past.Dec(location(), Past.Int, _2, None))
# 514 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 61 "parser.mly"
                                                    (Past.Dec(location(), Past.Int, _2, Some(_4)))
# 522 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 62 "parser.mly"
                                                    (Past.Dec(location(), Past.Bool, _2, None))
# 529 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 63 "parser.mly"
                                                    (Past.Dec(location(), Past.Bool, _2, Some(_4)))
# 537 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 64 "parser.mly"
                                                    (Past.Dec(location(), Past.Cell, _2, None))
# 544 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 65 "parser.mly"
                                                    (Past.Dec(location(), Past.Cell, _2, Some(_4)))
# 552 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 66 "parser.mly"
                                                    (Past.Dec(location(), Past.Region, _2, None))
# 559 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 67 "parser.mly"
                                                    (Past.Dec(location(), Past.Region, _2, Some(_4)))
# 567 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 68 "parser.mly"
                                                    (Past.Dec(location(), Past.Line, _2, None))
# 574 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 69 "parser.mly"
                                                    (Past.Dec(location(), Past.Line, _2, Some(_4)))
# 582 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.data_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 70 "parser.mly"
                                                    (Past.Dec(location(), Past.Set(_2), _3, None))
# 590 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 3 : Past.data_type) in
    let _3 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 71 "parser.mly"
                                                    (Past.Dec(location(), Past.Set(_2), _3, Some(_5)))
# 599 "parser.ml"
               : 'dec))
; (fun __caml_parser_env ->
    Obj.repr(
# 74 "parser.mly"
                                            (Past.Grid)
# 605 "parser.ml"
               : 'group))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'list) in
    Obj.repr(
# 75 "parser.mly"
                                            (Past.Instance(Past.List(location(), _2)))
# 612 "parser.ml"
               : 'group))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 78 "parser.mly"
                                            ([_1])
# 619 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'list) in
    Obj.repr(
# 79 "parser.mly"
                                            (_1::_3)
# 627 "parser.ml"
               : 'list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    Obj.repr(
# 82 "parser.mly"
                                            (Past.Utils(location(), _1, Past.Cells))
# 634 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    Obj.repr(
# 83 "parser.mly"
                                            (Past.Utils(location(), _1, Past.Value))
# 641 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    Obj.repr(
# 84 "parser.mly"
                                            (Past.Utils(location(), _1, Past.Size))
# 648 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    Obj.repr(
# 85 "parser.mly"
                                            (Past.Utils(location(), _1, Past.Length))
# 655 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 86 "parser.mly"
                                            (_1)
# 662 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 87 "parser.mly"
                                            (_2)
# 669 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'dec) in
    Obj.repr(
# 88 "parser.mly"
                                            (_1)
# 676 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 89 "parser.mly"
                                            (Past.Op(location(), _1, Past.Add, _3))
# 684 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 90 "parser.mly"
                                            (Past.Op(location(), _1, Past.Sub, _3))
# 692 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 91 "parser.mly"
                                            (Past.Op(location(), _1, Past.Mul, _3))
# 700 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 92 "parser.mly"
                                            (Past.Op(location(), _1, Past.Div, _3))
# 708 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 93 "parser.mly"
                                            (Past.UnaryOp(location(), Past.Neg, _2))
# 715 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 94 "parser.mly"
                                            (Past.Op(location(), _1, Past.And, _3))
# 723 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 95 "parser.mly"
                                            (Past.Op(location(), _1, Past.Or, _3))
# 731 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 96 "parser.mly"
                                            (Past.Op(location(), _1, Past.Xor, _3))
# 739 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 97 "parser.mly"
                                            (Past.UnaryOp(location(), Past.Not, _2))
# 746 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 98 "parser.mly"
                                            (Past.Op(location(), _1, Past.Equal, _3))
# 754 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 99 "parser.mly"
                                            (Past.Op(location(), _1, Past.LT, _3))
# 762 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 100 "parser.mly"
                                            (Past.Op(location(), _1, Past.GT, _3))
# 770 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 101 "parser.mly"
                                            (Past.Op(location(), _1, Past.LTE, _3))
# 778 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 102 "parser.mly"
                                            (Past.Op(location(), _1, Past.GTE, _3))
# 786 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 103 "parser.mly"
                                            (Past.Op(location(), _1, Past.Unequal, _3))
# 794 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 104 "parser.mly"
                                            (Past.Op(location(), _1, Past.LeftImp, _3))
# 802 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 105 "parser.mly"
                                            (Past.Op(location(), _1, Past.RightImp, _3))
# 810 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 106 "parser.mly"
                                            (Past.Op(location(), _1, Past.BiImp, _3))
# 818 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'group) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 108 "parser.mly"
                                            (Past.Quantifier(location(), Past.ForAll, _2, _4, _7))
# 827 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 6 : 'dec) in
    let _4 = (Parsing.peek_val __caml_parser_env 4 : 'group) in
    let _7 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 110 "parser.mly"
                                            (Past.Quantifier(location(), Past.Exists, _2, _4, _7))
# 836 "parser.ml"
               : Past.expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : int) in
    let _4 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 113 "parser.mly"
                                            ((location(), _2, _4))
# 844 "parser.ml"
               : 'init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'init) in
    Obj.repr(
# 114 "parser.mly"
                                            (_1)
# 851 "parser.ml"
               : 'init))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Past.expr) in
    Obj.repr(
# 117 "parser.mly"
                                            ([_1])
# 858 "parser.ml"
               : Past.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Past.expr) in
    Obj.repr(
# 118 "parser.mly"
                                            ([_1])
# 865 "parser.ml"
               : Past.expr list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : Past.expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : Past.expr list) in
    Obj.repr(
# 119 "parser.mly"
                                            (_1::_3)
# 873 "parser.ml"
               : Past.expr list))
; (fun __caml_parser_env ->
    Obj.repr(
# 122 "parser.mly"
                                            (Past.Int)
# 879 "parser.ml"
               : Past.data_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 123 "parser.mly"
                                            (Past.Bool)
# 885 "parser.ml"
               : Past.data_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 124 "parser.mly"
                                            (Past.Cell)
# 891 "parser.ml"
               : Past.data_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 125 "parser.mly"
                                            (Past.Region)
# 897 "parser.ml"
               : Past.data_type))
; (fun __caml_parser_env ->
    Obj.repr(
# 126 "parser.mly"
                                            (Past.Line)
# 903 "parser.ml"
               : Past.data_type))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : Past.data_type) in
    Obj.repr(
# 127 "parser.mly"
                                            (Past.Set(_2))
# 910 "parser.ml"
               : Past.data_type))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : ((Lexing.position * int * int) * Past.expr list))
