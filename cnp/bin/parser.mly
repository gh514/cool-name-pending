
%{

let location = Parsing.symbol_start_pos;;

%}


%token <int> INT
%token <string> VAR
%token TRUE FALSE
%token GRID CROSS CELL REGION CENTRELINE EDGELINE CENTRELOOP EDGELOOP R C INTDEC BOOLDEC ROW COLUMN BOX CELLDEC
%token ADD SUB MUL DIV ABS DIFF
%token AND OR NOT XOR IF THEN ELSE
%token EQUAL LT GT LTE GTE UNEQUAL MEMBER ADJACENT
%token LEFTIMP RIGHTIMP BIIMP
%token LBRACK RBRACK LSBRACK RSBRACK SEMICOLON POINT TO COMMA HALF
%token FORALL EXISTS NFORALL NEXISTS IN ARE
%token SIZE LENGTH SUM
%token DISTINCT EQUIVALENT
%token EOF HIGH LOW

%nonassoc LOW

%nonassoc IF THEN ELSE
%left BIIMP
%left LEFTIMP RIGHTIMP
%left LT GT LTE GTE UNEQUAL EQUAL NOT
%left MUL DIV OR XOR COMMA ROW
%left ABS DIFF
%left ADD SUB AND MEMBER ADJACENT

%right TO

%nonassoc INTDEC BOOLDEC CELL REGION CENTRELINE EDGELINE CROSS INT
%nonassoc LBRACK RBRACK LSBRACK RSBRACK GRID VAR
%nonassoc POINT R C

%nonassoc HIGH

%start main
%type <((Lexing.position * int * int) * Past.expr list)> main
%type <Past.expr> simple_expr
%type <Past.expr> expr

%%main:
    | init expr_list EOF                    {($1, $2)}
    | init EOF                              {($1, [])}

simple_expr:
    | TRUE                                  {Past.Boolean(location(), true)}
    | FALSE                                 {Past.Boolean(location(), false)}
    | INT                                   {Past.Integer(location(), $1)}
    | VAR                                   {Past.Var(location(), $1)}
    | R simple_expr C simple_expr %prec HIGH                        {Past.RC(location(), $2, $4)}
    | simple_expr TO simple_expr %prec LOW           {Past.Range(location(), $1, $3)}
    | group                                 {Past.Group(location(), $1)}
    | simple_expr HALF                      {Past.Corner(location(), $1)}
    | LBRACK expr RBRACK                    {$2}

datatype:   
    | INTDEC                                {Past.Int}
    | BOOLDEC                               {Past.Bool}
    | CELL                                  {Past.Cell}
    | REGION                                {Past.Region}
    | CENTRELINE                            {Past.CentreLine}
    | EDGELINE                              {Past.EdgeLine}
    | CENTRELOOP                            {Past.CentreLoop}
    | EDGELOOP                              {Past.EdgeLoop}
    | BOX                                   {Past.Box}

dec:
    | datatype list EQUAL simple_expr       {Past.Dec(location(), $1, Past.List(location(), $2), Some($4))}
    | datatype list                         {Past.Dec(location(), $1, Past.List(location(), $2), None)}

group:
    | GRID                                  {Past.Grid}
    | LSBRACK list RSBRACK                  {Past.Instance(Past.List(location(), $2))}
    | VAR                                   {Past.Instance(Past.Var(location(), $1))}
    | ROW                                   {Past.Row(None)}
    | ROW expr %prec HIGH                   {Past.Row(Some $2)}
    | COLUMN                                {Past.Column(None)}
    | COLUMN expr %prec HIGH                {Past.Column(Some $2)}
    | BOX                                   {Past.Boxes(None)}
    | REGION                                {Past.Regions}
    | LBRACK group RBRACK                   {$2}

list:
    | simple_expr                           {[$1]}
    | simple_expr COMMA list                {$1::$3}    

utils:
    | POINT CELL                            {Past.Cells}
    | POINT SIZE                            {Past.Size}
    | POINT LENGTH                          {Past.Length}
    | POINT REGION                          {Past.Reg}
    | POINT SUM                             {Past.Sum}

quantifier:
    | FORALL                                {Past.ForAll}
    | EXISTS                                {Past.Exists}
    | NFORALL                               {Past.NForAll}
    | NEXISTS                               {Past.NExists}

constraints:
    | DISTINCT                              {Past.Distinct}
    | EQUIVALENT                            {Past.Equivalent}

expr:
    | simple_expr                           {$1}
    | dec                                   {$1}
    | expr ADD expr                         {Past.Op(location(), $1, Past.Add, $3)}
    | expr SUB expr                         {Past.Op(location(), $1, Past.Sub, $3)}
    | expr MUL expr                         {Past.Op(location(), $1, Past.Mul, $3)}
    | expr DIV expr                         {Past.Op(location(), $1, Past.Div, $3)}
    | ABS expr                              {Past.UnaryOp(location(), Past.Abs, $2)}
    | SUB expr                              {Past.UnaryOp(location(), Past.Neg, $2)}
    | DIFF expr expr                        {Past.UnaryOp(location(), Past.Abs, Past.Op(location(), $2, Past.Sub, $3))}
    | expr AND expr                         {Past.Op(location(), $1, Past.And, $3)}
    | expr OR expr                          {Past.Op(location(), $1, Past.Or, $3)}
    | expr XOR expr                         {Past.Op(location(), $1, Past.Xor, $3)}
    | IF expr THEN expr ELSE expr           {Past.ITE(location(), $2, $4, $6)}
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
    | expr utils                            {Past.Utils(location(), $1, $2)}
    | simple_expr ADJACENT simple_expr MEMBER simple_expr %prec HIGH  
                                            {Past.SpecOp(location(), $1, Past.Adjacent(Some $5), $3)}
    | simple_expr ADJACENT simple_expr %prec LOW     {Past.SpecOp(location(), $1, Past.Adjacent(None), $3)}
    | expr ADJACENT CROSS INT expr          {Past.SpecOp(location(), $1, Past.CellLineAdjacent($4), $5)}
    | quantifier dec MEMBER group POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), $1, $2, $4, $7)}
    | quantifier dec POINT LBRACK expr RBRACK
                                            {Past.Quantifier(location(), $1, $2, Past.Grid, $5)}
    | list MEMBER simple_expr               {Past.Member(location(), Past.List(location(), $1), $3)}
    | datatype MEMBER group ARE constraints {Past.Sugar(location(), $1, Past.Group(location(), $3), $5)}
    | CELLDEC expr                          {Past.CellDec(location(), $2)}



init:
    | GRID INT CROSS INT                    {(location(), $2, $4)}
    | init SEMICOLON                        {$1}

expr_list:
    | expr                                  {[$1]}
    | expr SEMICOLON                        {[$1]}   
    | expr SEMICOLON expr_list              {$1::$3}

