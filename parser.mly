
%{

let location = Parsing.symbol_start_pos;;

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
%left EQUAL LT GT LTE GTE UNEQUAL NOT
%left LEFTIMP RIGHTIMP
%left BIIMP

%nonassoc LBRACK RBRACK LSBRACK RSBRACK

%start main
%type <Past.expr> main
%type <Past.expr> simple_expr
%type <Past.expr> expr
%type <Past.expr list> expr_list
%type <Past.data_type> data_type

%%main:
    expr_list EOF                       {Past.Seq(location(), $1)}

simple_expr:
    | TRUE                              {Past.Boolean(location(), true)}
    | FALSE                             {Past.Boolean(location(), false)}
    | INT                               {Past.Integer(location(), $1)}
    | VAR                               {Past.Var(location(), $1)}
    | GRID                              {Past.Grid(location())}
    | CELL simple_expr                  {Past.Dec(location(), Past.Cell, $2)}
    | LINE simple_expr                  {Past.Dec(location(), Past.Line, $2)}
    | REGION simple_expr                {Past.Dec(location(), Past.Region, $2)}
    | SET data_type simple_expr              {Past.Dec(location(), Past.Set($2), $3)}

    

expr:
    | simple_expr POINT CELLS           {Past.Utils(location(), $1, Past.Cells)}
    | simple_expr POINT VALUE           {Past.Utils(location(), $1, Past.Value)}
    | simple_expr POINT SIZE            {Past.Utils(location(), $1, Past.Size)}
    | simple_expr POINT LENGTH          {Past.Utils(location(), $1, Past.Length)}
    | simple_expr                                                   {$1}
    | expr ADD expr                                                 {Past.Op(location(), $1, Past.Add, $3)}
    | expr SUB expr                                                 {Past.Op(location(), $1, Past.Sub, $3)}
    | expr MUL expr                                                 {Past.Op(location(), $1, Past.Mul, $3)}
    | expr DIV expr                                                 {Past.Op(location(), $1, Past.Div, $3)}
    | SUB expr                                                      {Past.UnaryOp(location(), Past.Neg, $2)}
    | expr AND expr                                                 {Past.Op(location(), $1, Past.And, $3)}
    | expr OR expr                                                  {Past.Op(location(), $1, Past.Or, $3)}
    | expr XOR expr                                                 {Past.Op(location(), $1, Past.Xor, $3)}
    | NOT expr                                                      {Past.UnaryOp(location(), Past.Not, $2)}
    | expr EQUAL expr                                               {Past.Op(location(), $1, Past.Equal, $3)}
    | expr LT expr                                                  {Past.Op(location(), $1, Past.LT, $3)}
    | expr GT expr                                                  {Past.Op(location(), $1, Past.GT, $3)}
    | expr LTE expr                                                 {Past.Op(location(), $1, Past.LTE, $3)}
    | expr GTE expr                                                 {Past.Op(location(), $1, Past.GTE, $3)}
    | expr UNEQUAL expr                                             {Past.Op(location(), $1, Past.Unequal, $3)}
    | expr LEFTIMP expr                                             {Past.Op(location(), $1, Past.LeftImp, $3)}
    | expr RIGHTIMP expr                                            {Past.Op(location(), $1, Past.RightImp, $3)}
    | expr BIIMP expr                                               {Past.Op(location(), $1, Past.BiImp, $3)}
    | LBRACK expr RBRACK                                            {$2}
    | FORALL simple_expr POINT LBRACK expr RBRACK                   {Past.Quantifier(location(), Past.ForAll, $2, $5, Past.Group(Past.Universe))}
    | EXISTS simple_expr POINT LBRACK expr RBRACK                   {Past.Quantifier(location(), Past.Exists, $2, $5, Past.Group(Past.Universe))}
    | FORALL simple_expr IN simple_expr POINT LBRACK expr RBRACK    {Past.Quantifier(location(), Past.ForAll, $2, $4, $7)}
    | EXISTS simple_expr IN simple_expr POINT LBRACK expr RBRACK    {Past.Quantifier(location(), Past.Exists, $2, $4, $7)}
    

expr_list:
    | expr                          {[$1]}
    | expr SEMICOLON expr_list      {$1::$3}

data_type:
    | CELL                          {Past.Cell}
    | REGION                        {Past.Region}
    | LINE                          {Past.Line}
    | SET data_type                      {Past.Set($2)}

