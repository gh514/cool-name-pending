

{
  open Parser
  open Lexing 

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}

let var_regex = ['a'-'z'] ['a'-'z' '0'-'9' '_' ''']*
let int_regex = ['0'-'9']+
let newline = ('\n'|'\r'|"\n\r")
let cell_regex = ['R'] int_regex ['C'] int_regex

rule token = parse
    | [' ' '\t']      {token lexbuf}
    | ("Grid")        {GRID}
    | ('X')           {CROSS}
    | ('R')           {R}
    | ('C')           {C}
    | ("Cell")        {CELL}
    | ("Cells")       {CELL}
    | ("Region")      {REGION}
    | ("Regions")     {REGION}
    | ("Centreline")  {CENTRELINE}
    | ("Centrelines") {CENTRELINE}
    | ("Edgeline")    {EDGELINE}
    | ("Edgelines")   {EDGELINE}
    | ("Centreloop")  {CENTRELOOP}
    | ("Centreloops") {CENTRELOOP}
    | ("Edgeloop")    {EDGELOOP}
    | ("Edgeloops")   {EDGELOOP}
    | ("Int")         {INTDEC}
    | ("Bool")        {BOOLDEC}
    | ("Row")         {ROW}
    | ("Rows")        {ROW}
    | ("Column")      {COLUMN}
    | ("Columns")     {COLUMN}
    | ("Box")         {BOX}
    | ("Boxes")       {BOX}
    | ('+')           {ADD}
    | ('-')           {SUB}
    | ('*')           {MUL}
    | ('/')           {DIV}
    | ("Abs")         {ABS}
    | ("True")        {TRUE}
    | ("False")       {FALSE}
    | ("And")         {AND}
    | ("Or")          {OR}
    | ("Not")         {NOT}
    | ("Xor")         {XOR}
    | ("If")          {IF}
    | ("Then")        {THEN}
    | ("Else")        {ELSE}
    | ('=')           {EQUAL}
    | ('<')           {LT}
    | ('>')           {GT}
    | ("<=")          {LTE}
    | (">=")          {GTE}
    | ("!=")          {UNEQUAL}
    | ("->")          {LEFTIMP}
    | ("<-")          {RIGHTIMP}
    | ("<->")         {BIIMP}
    | ('(')           {LBRACK}
    | (')')           {RBRACK}
    | ('[')           {LSBRACK}
    | (']')           {RSBRACK}
    | (';')           {SEMICOLON}
    | (',')           {COMMA}
    | ("Forall")      {FORALL}
    | ("Exists")      {EXISTS}
    | ("!Forall")     {NFORALL}
    | ("!Exists")     {NEXISTS}
    | ('.')           {POINT}
    | ("Size")        {SIZE}
    | ("Length")      {LENGTH}
    | ("Adj")         {ADJACENT}
    | ("Sum")         {SUM}
    | ("To")          {TO}
    | ("On")          {MEMBER}
    | ("In")          {MEMBER}
    | ("Are")         {ARE}
    | ("Distinct")    {DISTINCT}
    | ("Unique")      {DISTINCT}
    | ("Equal")       {EQUIVALENT}
    | ("Diff")        {DIFF}
    | (".5")          {HALF}
    | ("Cells Contain Digits") {CELLDEC}
    | int_regex       {INT (int_of_string (Lexing.lexeme lexbuf))}
    | var_regex       {VAR (Lexing.lexeme lexbuf)}
    | (newline)       {next_line lexbuf; token lexbuf}
    | eof             {EOF}
    | _ as c          {failwith (Printf.sprintf "unexpected character: %C" c)}