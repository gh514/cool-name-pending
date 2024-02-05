
%{

let location = Parsing.symbol_start_pos;;

%}


%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token GRID CROSS CELL REGION LINE ROW COLUMN INTDEC BOOLDEC
%token ADD SUB MUL DIV
%token AND OR NOT XOR
%token EQUAL LT GT LTE GTE UNEQUAL
%token LEFTIMP RIGHTIMP BIIMP
%token LBRACK RBRACK LSBRACK RSBRACK SEMICOLON COMMA POINT
%token FORALL EXISTS NFORALL NEXISTS IN
%token CELLS VALUE SIZE LENGTH
%token ADJACENT
%token EOF

%left ADJACENT
%left BIIMP
%left LEFTIMP RIGHTIMP
%left LT GT LTE GTE UNEQUAL NOT
%left MUL DIV OR XOR
%left ADD SUB AND

%nonassoc INTDEC BOOLDEC CELL REGION LINE CROSS
%nonassoc LBRACK RBRACK LSBRACK RSBRACK EQUAL GRID

%start main
%type <((Lexing.position * int * int) * Past.expr list)> main
%type <Past.expr> simple_expr
%type <Past.expr> expr
%type <Past.expr list> expr_list

%%main:
    | init expr_list EOF                    {($1, $2)}
    | init EOF                              {($1, [])}

simple_expr:
    | TRUE                                  {Past.Boolean(location(), true)}
    | FALSE                                 {Past.Boolean(location(), false)}
    | INT                                   {Past.Integer(location(), $1)}
    | VAR                                   {Past.Var(location(), $1)}
    | ROW simple_expr COLUMN simple_expr    {Past.RC(location(), $2, $4)}
    | group                                 {Past.Group(location(), $1)}

dec:
    | INTDEC simple_expr                            {Past.Dec(location(), Past.Int, $2, None)}
    | INTDEC simple_expr EQUAL simple_expr          {Past.Dec(location(), Past.Int, $2, Some($4))}
    | BOOLDEC simple_expr                           {Past.Dec(location(), Past.Bool, $2, None)}
    | BOOLDEC simple_expr EQUAL simple_expr         {Past.Dec(location(), Past.Bool, $2, Some($4))}
    | CELL simple_expr                              {Past.Dec(location(), Past.Cell, $2, None)}
    | CELL simple_expr EQUAL simple_expr            {Past.Dec(location(), Past.Cell, $2, Some($4))}
    | REGION simple_expr                            {Past.Dec(location(), Past.Region, $2, None)}
    | REGION simple_expr EQUAL simple_expr          {Past.Dec(location(), Past.Region, $2, Some($4))}
    | LINE simple_expr                              {Past.Dec(location(), Past.Line, $2, None)}
    | LINE simple_expr EQUAL simple_expr            {Past.Dec(location(), Past.Line, $2, Some($4))}

group:
    | GRID                                  {Past.Grid}
    | LSBRACK list RSBRACK                  {Past.Instance(Past.List(location(), $2))}

list:
    | simple_expr                           {[$1]}
    | simple_expr COMMA list                {$1::$3}    

expr:
    | simple_expr POINT CELLS               {Past.Utils(location(), $1, Past.Cells)}
    | simple_expr POINT VALUE               {Past.Utils(location(), $1, Past.Value)}
    | simple_expr POINT SIZE                {Past.Utils(location(), $1, Past.Size)}
    | simple_expr POINT LENGTH              {Past.Utils(location(), $1, Past.Length)}
    | simple_expr                           {$1}
    | LBRACK expr RBRACK                    {$2}
    | dec                                   {$1}
    | expr ADD expr                         {Past.Op(location(), $1, Past.Add, $3)}
    | expr SUB expr                         {Past.Op(location(), $1, Past.Sub, $3)}
    | expr MUL expr                         {Past.Op(location(), $1, Past.Mul, $3)}
    | expr DIV expr                         {Past.Op(location(), $1, Past.Div, $3)}
    | SUB expr                              {Past.UnaryOp(location(), Past.Neg, $2)}
    | expr AND expr                         {Past.Op(location(), $1, Past.And, $3)}
    | expr OR expr                          {Past.Op(location(), $1, Past.Or, $3)}
    | expr XOR expr                         {Past.Op(location(), $1, Past.Xor, $3)}
    | NOT expr                              {Past.UnaryOp(location(), Past.Not, $2)}
    | expr EQUAL expr                       {Past.Op(location(), $1, Past.Equal, $3)}
    | expr LT expr                          {Past.Op(location(), $1, Past.LT, $3)}
    | expr GT expr                          {Past.Op(location(), $1, Past.GT, $3)}
    | expr LTE expr                         {Past.Op(location(), $1, Past.LTE, $3)}
    | expr GTE expr                         {Past.Op(location(), $1, Past.GTE, $3)}
    | expr UNEQUAL expr                     {Past.Op(location(), $1, Past.Unequal, $3)}
    | expr LEFTIMP expr                     {Past.Op(location(), $1, Past.LeftImp, $3)}
    | expr RIGHTIMP expr                    {Past.Op(location(), $1, Past.RightImp, $3)}
    | expr BIIMP expr                       {Past.Op(location(), $1, Past.BiImp, $3)}
    | VAR ADJACENT VAR                      {Past.RegionOp(location(), Past.Var(location(), $1), Past.Adjacent, Past.Var(location(), $3))}
    | FORALL dec IN group POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), Past.ForAll, $2, $4, $7)}
    | EXISTS dec IN group POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), Past.Exists, $2, $4, $7)}
    | FORALL dec POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), Past.ForAll, $2, Past.Grid, $5)}
    | EXISTS dec POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), Past.Exists, $2, Past.Grid, $5)}

init:
    | GRID INT CROSS INT                    {(location(), $2, $4)}
    | init SEMICOLON                        {$1}

expr_list:
    | expr                                  {[$1]}
    | expr SEMICOLON                        {[$1]}   
    | expr SEMICOLON expr_list              {$1::$3}

