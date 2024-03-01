
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
%token LBRACK RBRACK LSBRACK RSBRACK SEMICOLON COMMA POINT TO
%token FORALL EXISTS NFORALL NEXISTS IN
%token CELLS SIZE LENGTH SUM
%token ADJACENT
%token EOF

%left ADJACENT COMMA
%left BIIMP
%left LEFTIMP RIGHTIMP
%left LT GT LTE GTE UNEQUAL NOT
%left MUL DIV OR XOR
%left ADD SUB AND

%nonassoc POINT
%nonassoc INTDEC BOOLDEC CELL REGION LINE CROSS
%nonassoc LBRACK RBRACK LSBRACK RSBRACK EQUAL GRID VAR

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
    | simple_expr TO simple_expr            {Past.Range(location(), $1, $3)}

datatype:   
    | INTDEC                                {Past.Int}
    | BOOLDEC                               {Past.Bool}
    | CELL                                  {Past.Cell}
    | REGION                                {Past.Region}
    | LINE                                  {Past.Line}

dec:
    | datatype simple_expr                            {Past.Dec(location(), $1, Past.List(location(), [$2]), None)}
    | datatype simple_expr EQUAL simple_expr          {Past.Dec(location(), $1, Past.List(location(), [$2]), Some($4))}
    | datatype list                                   {Past.Dec(location(), $1, Past.List(location(), $2), None)}

group:
    | GRID                                  {Past.Grid}
    | LSBRACK list RSBRACK                  {Past.Instance(Past.List(location(), $2))}

list:
    | simple_expr                           {[$1]}
    | simple_expr COMMA list                {$1::$3}    

utils:
    | POINT CELLS                           {Past.Cells}
    | POINT SIZE                            {Past.Size}
    | POINT LENGTH                          {Past.Length}
    | POINT REGION                          {Past.Reg}
    | POINT SUM                             {Past.Sum}

quantifier:
    | FORALL                                {Past.ForAll}
    | EXISTS                                {Past.Exists}
    | NFORALL                               {Past.NForAll}
    | NEXISTS                               {Past.NExists}

expr:
    | simple_expr utils                     {Past.Utils(location(), $1, $2)}
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
    | expr ADJACENT expr                    {Past.RegionOp(location(), $1, Past.Adjacent, $3)}
    | quantifier dec IN group POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), $1, $2, $4, $7)}
    | quantifier dec POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), $1, $2, Past.Grid, $5)}

init:
    | GRID INT CROSS INT                    {(location(), $2, $4)}
    | init SEMICOLON                        {$1}

expr_list:
    | expr                                  {[$1]}
    | expr SEMICOLON                        {[$1]}   
    | expr SEMICOLON expr_list              {$1::$3}

