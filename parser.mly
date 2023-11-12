
%{
let location = Parsing.symbol_start_pos 
%}


%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token GRID CELL SET REGION LINE
%token ADD SUB MUL DIV
%token AND OR NOT XOR
%token EQUAL LT GT LTE GTE UNEQUAL
%token LEFTIMP RIGHTIMP BIIMP
%token LBRACK RBRACK LSBRACK RSBRACK SEMICOLON COMMA POINT
%token FORALL EXISTS NFORALL NEXISTS IN
%token CELLS VALUE SIZE LENGTH
%token ADJACENT
%token SPACE EOF

%left ADD SUB AND
%left MUL DIV OR XOR
%left EQUAL LT GT LTE GTE UNEQUAL
%left LEFTIMP RIGHTIMP
%left BIIMP

%nonassoc LBRACK RBRACK LSBRACK RSBRACK

%start main
%type <Past.expr> main
%type <Past.expr> simple_expr
%type <Past.expr> expr
%type <Past.expr list> expr_list

%%main:
    expr EOF                        {$1}

simple_expr:
    | TRUE                          {Past.Boolean(location(), true)}
    | INT                           {Past.Integer(location(), $1)}
    | VAR                           {Past.Var(location(), $1)}



expr:
    | INT                           {Past.Integer(location(), $1)}

expr_list:
    | expr                          {[$1]}
    | expr SEMICOLON expr_list      {$1::$3}