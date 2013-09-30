 %{

(*
 * 
 * Copyright (c) 2011 Matthieu Dubet
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
*)
        open Lang
 %}

%token <bool> BCONST
%token <int> INTCONST
%token <string> IDENTIFIER
%token <string> STRINGCONST
%token <string> META
%token <string> WHATEVER

%token SERVICE
%token CONST
%token HTML
%token HTML_TAG_START
%token HTML_TAG_END
%token META_START
%token META_END
%token INPUT
%token SELECT
%token TYPE
%token NAME
%token TEXT
%token RADIO
%token SCHEMA
%token SESSION
%token SHOW
%token EXIT
%token IF, ELSE, WHILE
%token FOR
%token RETURN
%token PLUG
%token RECEIVE


%token INT
%token BOOL
%token STRING
%token VOID
%token TUPLE


%token LEFTB
%token RIGHTB
%token ASSIGN
%token SEMICOLON
%token LT_SLASH
%token GT_SLASH
%token LT_BRACKET
%token GT_BRACKET
%token LEFTP
%token RIGHTP
%token LEFTBRACKET
%token RIGHTBRACKET
%token COMMA
%token KEEP
%token REMOVE
%token QUOTE
%token JOIN
%token EQ, LT, LE, GT, GE, NEQ
%token NOT
%token PLUS, MINUS
%token MULT, DIV, MOD
%token AND, OR
%token DOT

%token EOF

%nonassoc THEN
%nonassoc ELSE
%right ASSIGN
%left AND
%left OR
%left EGAL, EQ, NEQ, LT, LE, GT, GE
%left JOIN, KEEP, REMOVE
%left PLUS, MINUS
%left MULT, DIV, MOD
%nonassoc NOT

%type <Lang.inputtype> inputtype
%type <Lang.inputattr> inputattr
%type <Lang.inputattr list> inputattrs
%type <Lang.attribute list> attributes
%type <Lang.htmlbody list> htmlbody
%type <Lang.htmlbody list> htmlbodies
%type <Lang.html> html
%type <Lang.html list> htmls
%type <Lang.wig> wig
%start wig

%%

wig: 
  |SERVICE LEFTB htmls schemas varsfuns sessions RIGHTB 
  {
    Wig($3,$4,fst $5,snd $5,$6,Root) }
;

varsfuns:
  | { [], [] }
  | variable varsfuns { $1::fst $2, snd $2 }
  | functionr varsfuns { fst $2, $1::snd $2 }
;

html :
  CONST HTML IDENTIFIER ASSIGN HTML_TAG_START htmlbodies HTML_TAG_END SEMICOLON
  { ($3,$6) }
;

htmls :
  | htmls html { $1@[$2] }
  | html { $1::[] }
;

htmlbodies :
  | { [] }
  | htmlbodies htmlbody { $1@$2 }
  ;

htmlbody:
    | LT INPUT inputattrs GT { [Input($3)] }
    | LT INPUT inputattrs GT_SLASH { [Input($3)] }
    | LT IDENTIFIER attributes GT { [StartTagIdent($2,$3)] }
    | LT IDENTIFIER attributes GT_SLASH { [StartTagIdent($2,$3)] }
    | LT_SLASH IDENTIFIER GT { [EndTagIdent($2)] }
    | LT_BRACKET IDENTIFIER GT_BRACKET { [WigIdent($2)] }
    | WHATEVER { [Whatever($1)] }
    | META_START META META_END { [Meta($2)] }
;

inputattrs :
  | inputattrs inputattr {  $1@[$2] }
  | inputattr { $1::[] }
  ;

inputattr :
  | NAME ASSIGN STRINGCONST { Name($3)  }
  | TYPE ASSIGN inputtype { Type($3) }
  | attribute { Attribute($1) }
  ;

inputtype :
    | RADIO { Radio }
    | TEXT { Text }
    ;

attributes :
    | { [] }
    | attributes attribute { $1@[$2] }
    ;

attribute :
   | attr ASSIGN attr { KeyValue($1,$3) }
   | attr { Single($1) }
   ;

attr :
  | IDENTIFIER { Identifier($1) }
  | STRINGCONST { Stringconst($1) }
  | INTCONST { Intconst($1) }
  | LT_BRACKET IDENTIFIER GT_BRACKET { WigAttr($2) }
  ;
 schemas :
    | { [] }
    | schemas schema { $1@[$2] }
    ;

    schema :
      | SCHEMA  IDENTIFIER LEFTB fields RIGHTB { ($2,$4) }
      ;

      fields :
    | { [] }
    | fields field { $1@[$2] }
    ;

    field:
      | simpletype IDENTIFIER  SEMICOLON { ($1,$2) }
      ;

      simpletype: 
        | INT { Int }
        | BOOL { Bool }
        | VOID { Void }
        | STRING { String }
        ;

 variables :
    | { [] }
    | variables variable { $1@[$2] }
    ;


 variable:
   |  typer identifiers SEMICOLON { ($1,$2) }
  ;

   identifiers :
     | IDENTIFIER { $1::[] }
     | IDENTIFIER COMMA identifiers { $1::$3 }
  ;

  typer : 
    | simpletype { SimpleType($1) }
    | TUPLE  IDENTIFIER { TupleType(Static($2)) }
    ; 


    functionr :
      | typer IDENTIFIER LEFTP arguments RIGHTP cstm { ($1,$2,$4,$6,Root) }
      ;


    arguments:
    | { [] }
    | argument { [$1] }
    | argument COMMA arguments { $1::$3 }
    ;

    argument:
      | typer IDENTIFIER  { ($1,$2) }
  ;
    
sessions :
  | sessions session { $1@[$2] }
  | session { $1::[] }
;

session :
  | SESSION IDENTIFIER LEFTP RIGHTP cstm { ($2,$5) }
  ;

  stms:
    | { [] }
    | stms stm { $1@[$2] }
    ;

    stm:
      | SEMICOLON  { Skip }
      | SHOW document receive  SEMICOLON { Show($2,$3) }
      | EXIT exitdocument  SEMICOLON { Exit($2) }
      | RETURN SEMICOLON { ReturnVoid } 
      | RETURN exp SEMICOLON { Return($2) }
      | IF LEFTP exp  RIGHTP stm %prec THEN { If($3,$5) }
      | IF LEFTP exp RIGHTP stm  ELSE stm { IfElse($3,$5,$7) }
      | WHILE LEFTP exp RIGHTP stm { While($3,$5) }
      | cstm { Comp($1) }
      | exp SEMICOLON  { Expr($1) }
      ;
     

   document: 
      | IDENTIFIER { DocumentId($1) }
      | PLUG IDENTIFIER LEFTBRACKET plugs RIGHTBRACKET { Plug($2,$4) }
      ;

   exitdocument: 
      | IDENTIFIER { DocumentId($1) }
      | IDENTIFIER LEFTBRACKET plugs RIGHTBRACKET { Plug($1,$3) }
      | PLUG IDENTIFIER LEFTBRACKET plugs RIGHTBRACKET { Plug($2,$4) }
      ;

   receive:
     |  { DontReceive }
     |  RECEIVE LEFTBRACKET inputs RIGHTBRACKET { Receive($3) }
     ;

     cstm :
  | LEFTB variables stms RIGHTB { ($2,$3,Root) }     
  ;

plugs :
  | plug COMMA plugs { $1::$3 }
  | plug { $1::[] }
;
plug :
 | IDENTIFIER ASSIGN exp { ( $1, $3) }
 ;
 inputs:
    | { [] }
    | input { [$1] }
    | input COMMA inputs { $1::$3 }
    ;

    input :
      lvalue  ASSIGN IDENTIFIER { ($1,$3) }
      ;

      exp :
          | LEFTP exp RIGHTP { $2 }
          | lvalue { Lval($1) }
          | lvalue ASSIGN exp {  LvalAssign($1,$3) }
          | exp EQ exp { Eq($1,$3) }
          | exp NEQ exp { Neq($1,$3) }
          | exp LT exp { Lt($1,$3) }
          | exp GT exp { Gt($1,$3) }
          | exp LE exp { Le($1,$3) }
          | exp GE exp { Ge($1,$3) }
          | NOT exp { Not($2) }
          | MINUS exp { Opposite($2) }
          | exp PLUS exp { Plus($1,$3) }
          | exp MINUS exp { Minus($1,$3) }
          | exp MULT exp { Mult($1,$3) }
          | exp DIV exp { Div($1,$3) }
          | exp MOD exp { Mod($1,$3) }
          | exp AND exp { And($1,$3) }
          | exp OR exp { Or($1,$3) }
          | exp JOIN exp { Join($1,$3) }
          | exp KEEP IDENTIFIER { Keep($1,[$3]) }
          | exp REMOVE IDENTIFIER { Remove($1,[$3]) }
          | exp KEEP LEFTP identifiers RIGHTP { Keep($1,$4) }
          | exp REMOVE LEFTP identifiers RIGHTP { Remove($1,$4) }
          | IDENTIFIER LEFTP exps RIGHTP { Apply($1,$3) }
          | INTCONST { IntConst($1) }
          | BCONST { BoolConst($1) }
          | STRINGCONST { StringConst($1) }
          | TUPLE LEFTB fieldvalues RIGHTB { TupleExp($3) }
          ;

          exps :
            | { [] }
            | exp { [$1] }
            | exp COMMA exps { $1::$3 }
            ;

            lvalue :
              | IDENTIFIER { ValId($1) }
              | IDENTIFIER DOT IDENTIFIER  { ValComp($1,$3) }
              ;

          fieldvalues :
            |  fieldvalue { $1::[] }
            |  fieldvalue COMMA fieldvalues { $1::$3 }
            ;

            fieldvalue :
              | IDENTIFIER ASSIGN exp { ($1,$3) }
