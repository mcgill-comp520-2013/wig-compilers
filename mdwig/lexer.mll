(*
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
{
open Parser
  exception Lexerror
}

let space  = [' ''\t']+
let blank  = [' ''\t''\n']+
let num    = ('0'|['1'-'9']['0'-'9']*)
let identifier  = ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''0'-'9''_']*
let stringconst  = '"' ([^'"']| '\\' '"')*'"'
let meta = ([^'/']|'/'[^'>'])*
let htmlcomment = "<!--"([^'-']|'-'[^'-']|'-''-'[^'>'])*"-->"
let whatever = [^'<''>']+
let cstylecommentmulti = "/*"([^'*']|'*'[^'/'])*"*/"
let cstylecomment = "//"[^'\n']*'\n'

        rule token = parse
        | space        { token lexbuf }
        | cstylecomment { token lexbuf }
        | cstylecommentmulti { token lexbuf }
        | "service"     {[ SERVICE ]}
        | "const"      {[ CONST ]}
        | "html"       {[ HTML ]}
        | "<html>"     { HTML_TAG_START::htmlparse lexbuf }
        | "schema"	{[ SCHEMA ]}
        | "session"	{[ SESSION ]}
        | "show"	{[ SHOW ]}
        | "exit"	{[ EXIT ]}
        | "if"          {[ IF ]}
        | "else"        {[ ELSE ]}
        | "while"       {[ WHILE ]}
        | "for"         {[ FOR ]}
        | "return"      {[ RETURN ]}
        | "plug"	{[ PLUG ]}
        | "receive"	{[ RECEIVE ]}


        | "int"		{[ INT ]}
        | "bool"	{[ BOOL ]}
        | "string"	{[ STRING ]}
        | "void"	{[ VOID ]}
        | "tuple"	{[ TUPLE ]}
        | "true" { [ (BCONST true) ] }
        | "false" { [ (BCONST false) ] }

        | '{'           {[ LEFTB ]}
      	| '}'           {[ RIGHTB ]}
        | '='           {[ ASSIGN ]}
        | ';'           {[ SEMICOLON ]}
        | '<'           {[ LT ]}
        | '>'           {[ GT ]}
        | '('           {[ LEFTP ]}
        | ')'           {[ RIGHTP ]}
        | '['		{[ LEFTBRACKET ]}
        | ']'		{[ RIGHTBRACKET ]}
        | ','		{[ COMMA ]}
        | "\\+"		{[ KEEP ]}
        | "\\-"		{[ REMOVE ]}
        | "<<"		{[ JOIN ]}
        | "=="          {[ EQ ]}
        | "!="		{[ NEQ ]}
        | "<="          {[ LE ]}
        | ">="          {[ GE ]}
        | '!'		{[ NOT ]}
        | '-'            {[ MINUS ]}
        | '+'            {[ PLUS ]}
        | '*'           {[ MULT ]}
        | '/'           {[ DIV ]}
        | '%'           {[ MOD ]}
        | "&&"          {[ AND ]}
        | "||"          {[ OR ]}
        | '.'		        {[ DOT ]}
        | '"'		        { [QUOTE] }

        | stringconst as str {[ STRINGCONST str ]}
        | num as i {[ INTCONST (int_of_string i)]}
        | identifier as str {[ IDENTIFIER str ]}
        | '\n'           { token lexbuf }
        | eof            {[ EOF ]}
        | _              { Printf.printf "[Lexer error] unrecogized symbol : %s /FIN SYMBOL\n"
                            (Lexing.lexeme lexbuf);
                          raise Lexerror } 


        and htmlparse = parse
        | blank        { htmlparse lexbuf }
        | htmlcomment  { htmlparse lexbuf } 
        | "<meta"  { META_START::metaparse lexbuf } 
        | "</html>"   { HTML_TAG_END::token lexbuf }
        | "<["		{ LT_BRACKET::holeparse lexbuf }
        | "</"		{ LT_SLASH::endtagparse lexbuf }
        | "<input"		{ LT::INPUT::inputparse lexbuf }
        | '<' { LT::tagparse lexbuf }
        | whatever as w { (WHATEVER w)::htmlparse lexbuf}
        | _              { Printf.printf "[Lexer error in HTML parse] unrecogized symbol '%s'\n"
        (Lexing.lexeme lexbuf);
        raise Lexerror } 


      and inputparse = parse
        | blank        { inputparse lexbuf }
        | "name" { NAME::inputparse lexbuf }
        | "type" { TYPE::inputparse lexbuf }
        | "/>" { GT_SLASH::htmlparse lexbuf }
        | '>' { GT::htmlparse lexbuf }
        | '=' { ASSIGN::inputrightvalparse lexbuf }
        | "select"	{SELECT::tagparse lexbuf}
        | identifier as str {(IDENTIFIER str)::inputparse lexbuf }
        | _              {
		    Printf.printf 
		    "[Lexer error in INPUT TAG parse] Token unrecognized : '%s'\n"
		    (Lexing.lexeme lexbuf);
		    raise Lexerror} 


      and inputrightvalparse = parse
        | blank        { inputrightvalparse lexbuf }
        | "\"text\""	{ TEXT::inputparse lexbuf}
        | "\"radio\""	{ RADIO::inputparse lexbuf}
        | stringconst as str { (STRINGCONST str)::inputparse lexbuf }
        | num as i {(INTCONST (int_of_string i))::inputparse lexbuf }
        | identifier as str {(IDENTIFIER str)::inputparse lexbuf }
        | _              {
		    Printf.printf 
		    "[Lexer error] Input right value is unrecognized : '%s'\n"
		    (Lexing.lexeme lexbuf);
		    raise Lexerror} 

       and tagparse = parse
        | blank        { tagparse lexbuf }
        | '>' { GT::htmlparse lexbuf }
        | '/' { tagparse lexbuf }
        | '=' { ASSIGN::rightvalparse lexbuf }
        | identifier as str {(IDENTIFIER str)::tagparse lexbuf }
        | stringconst as str { (STRINGCONST str)::tagparse lexbuf }
        | num as i {(INTCONST (int_of_string i))::tagparse lexbuf }
        | _              { Printf.printf "[Lexer error in HTML tag parse] unrecogized symbol '%s'\n"
        (Lexing.lexeme lexbuf);
        raise Lexerror } 

      and endtagparse = parse
        | '>' { GT::htmlparse lexbuf }
        | identifier as str {(IDENTIFIER str)::endtagparse lexbuf }
        | _              { Printf.printf "[Lexer error in HTML endtag parse] unrecogized symbol '%s'\n"
        (Lexing.lexeme lexbuf);
        raise Lexerror } 

      and rightvalparse = parse
        | blank        { rightvalparse lexbuf }
        | "<["		{ LT_BRACKET::holetagparse lexbuf }
        | stringconst as str { (STRINGCONST str)::tagparse lexbuf }
        | num as i {(INTCONST (int_of_string i))::tagparse lexbuf }
        | identifier as str {(IDENTIFIER str)::tagparse lexbuf }
        | _              {
		    Printf.printf 
		    "[Lexer error] Right value is unrecognized : '%s'\n"
		    (Lexing.lexeme lexbuf);
		    raise Lexerror} 

      and holeparse = parse
        | "]>"  { GT_BRACKET::htmlparse lexbuf }
        | identifier as i { (IDENTIFIER i)::holeparse lexbuf }
        | _              { Printf.printf "[Lexer error] Invalid named for a hole should be <[id]> instead of  '%s'\n"
        (Lexing.lexeme lexbuf);
        raise Lexerror} 

      and holetagparse = parse
        | "]>"  { GT_BRACKET::tagparse lexbuf }
        | identifier as i { (IDENTIFIER i)::holetagparse lexbuf }
        | _              { Printf.printf "[Lexer error] Invalid named for a hole should be <[id]> instead of  '%s'\n"
        (Lexing.lexeme lexbuf);
        raise Lexerror} 

        and metaparse = parse
       (*// | blank  { metaparse lexbuf } *)
        | "/>" {  META_END::htmlparse lexbuf }
        | meta as m { (META m)::metaparse lexbuf }
        | _              { Printf.printf "[Lexer error in HTML META parse] unrecogized symbol '%s'\n"
        (Lexing.lexeme lexbuf);
        raise Lexerror } 

