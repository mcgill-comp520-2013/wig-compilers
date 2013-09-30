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

open Lang

(* Escape color *)
let couleur c  = print_string ("\033[" ^ c ^ "m")
let noir = "30"
let fin =  "0"

(* Print functions *)
let simpletype_to_string = function
    | Int -> "int "
    | Bool -> "bool "
    | String -> "string "
    | Void -> "void "
      
	
let rec typet_to_string = function
      | SimpleType(s) -> simpletype_to_string s
      | TupleType(tuple) ->  "tuple " ^ tuple_to_string tuple ^ " "

and tuple_to_string = function
    | Static n -> "of name " ^ n
    | Dynamic fieldl -> fields_to_string fieldl

and string_list_separated l to_string_fun separation =
match l with
  | [] -> ""
  | h::[] -> (to_string_fun h)
  | h::t -> (to_string_fun h) ^ separation ^ ( string_list_separated t to_string_fun separation)

and argument_to_string = function
    | (typet,name) -> (typet_to_string typet ) ^ name

and field_to_string = function
    | (a,b) -> simpletype_to_string a ^ " " ^ b ^ " "

and fields_to_string x =
    List.fold_left
       ( fun init field -> init ^ (field_to_string field) )
       ""
       x

let htval_to_string = function
  | ST_Html(inputl,outputl) -> "HTML const which receive[" ^ (string_list_separated
  inputl (fun x -> x)  ", " ) ^  "] and send [" ^ (string_list_separated
  outputl (fun x -> x) ", " ) ^ "] "
  | ST_Variable t -> "variable of type " ^ typet_to_string t
  | ST_Func(returnt,args) -> "function which return " ^ typet_to_string
  returnt ^ " with args " ^ (string_list_separated args typet_to_string ", " )
  | ST_Schema(fields) -> "tuple" ^ " ( " ^ fields_to_string fields ^ " )"
    
let rec print psymbol ptype wig =  match wig with
  | Wig(htmls,schemas,variables,functions, sessions, st) ->
      (if psymbol then (symboltable_to_string st) else "") ^ "service {\n" ^ 
  (List.fold_left ( fun x y -> x ^ "\n" ^ (html_to_string y))  "" htmls ) ^
  (List.fold_left ( fun x y ->  x ^ "\n" ^(schema_to_string y) )  "" schemas ) ^
  (List.fold_left ( fun x y ->  x ^ "\n" ^(variable_to_string y) ) ""  variables ) ^
  (List.fold_left ( fun x y ->  x ^ "\n" ^(function_to_string y psymbol ptype) ) ""  functions ) ^
  (List.fold_left ( fun x y ->  x ^ "\n" ^(session_to_string y psymbol ptype) )  "" sessions ) ^
  "}\n" 


and ht_to_string ht =
  Hashtbl.fold (fun key value init ->
    key ^ " => " ^ htval_to_string value ^ "\n" ^ init )
  ht
  ""


and symboltable_to_string = function
  | Ht (ht,parent) ->
    "\n------- Symbol Table ---------" ^ ( symboltable_to_string parent ) ^ (ht_to_string ht) ^ "--------------------------------\n"
  | Root -> "\n"
    
and html_to_string = function
  | (name,bodies) ->
   "const html " ^ name ^ " =  <html>" ^
    (List.fold_left
       (fun init x -> init ^ (htmlbody_to_string x) )
       ""
       bodies
    )
    ^ "</html> ;\n"
      
and htmlbody_to_string = function
  | StartTagIdent(name,attrs)  ->  "<" ^ name ^ " " ^ (string_list_separated attrs attribute_to_string " " ) ^ ">"
  | EndTagIdent(name) ->  "</" ^ name ^ ">"
  | WigIdent(name) -> "<[" ^ name ^ "]>"
  | Whatever(content) -> content
  | Meta(content) -> "<meta " ^ content ^ " />"
  | Input(inputattrs) -> "<input " ^
    (List.fold_left
       (fun init input -> init ^ (inputattr_to_string input) )
       ""
       inputattrs
    )
    ^ ">"
      

and attribute_to_string = function
  | Single(a) -> attr_to_string  a
  | KeyValue(a,b) -> attr_to_string a ^ " = " ^ attr_to_string b 
    

and inputattr_to_string = function
  | Name(a) -> "name=" ^  a
  | Type(t) -> "type=" ^ ( match t with 
                                          | Radio -> "\"radio\""
                                          | Text -> "\"text\"" )
  | Attribute(x) -> attribute_to_string x

    
and attr_to_string = function
  | Identifier(i) -> i
  | Stringconst(s) -> s
  | Intconst(i) -> string_of_int i
  | WigAttr(s) -> "<[" ^ s ^ "]>"

and schema_to_string = function
  | (name,fields) -> " schema " ^ name ^ " {\n" ^ (fields_to_string fields)
    ^ "}\n"


and variable_to_string = function
    | (t,identifierl) -> typet_to_string t ^
      ( string_list_separated identifierl (fun x -> x ) " , ")
      ^ " ;"

and session_to_string s psymbol ptype = match s with
  | (name,cstm) -> "session " ^ name ^ "()"  ^ (cstm_to_string cstm psymbol
  ptype)

and cstm_to_string cstm psymbol ptype = match cstm with
  | (varl,stml,st) ->  (if psymbol then (symboltable_to_string st) else "")  ^  "\n{\n" ^
    (List.fold_left
       ( fun init y -> init ^ "\n" ^ (variable_to_string y)  )
       ""
       varl
    ) ^ 
    (List.fold_left
       ( fun init y -> init ^ "\n" ^(stm_to_string y psymbol ptype ) )
       ""
       stml
    )
    ^ "\n}\n"

and stm_to_string stm psymbol ptype = match stm with
      | Show (document,receive) ->
	"show " ^ (document_to_string document) ^ " " ^ (receive_to_string receive) ^ " ;\n"
      | Exit (document) ->
	"exit " ^ (document_to_string document) ^ " ;\n"
      | ReturnVoid ->
	"return ;\n"
      | Return(exp) ->
	"return " ^ (exp_to_string exp) ^ " ;\n"
      | If (exp,stm) ->
	"if ( " ^ (exp_to_string exp)  ^ " ) " ^  (stm_to_string stm psymbol ptype) 
	  
      | IfElse (exp,stm1,stm2) ->
	"if ( " ^ (exp_to_string exp) ^ " ) " ^  ( stm_to_string stm1 psymbol ptype) ^
  " else " ^ stm_to_string stm2 psymbol ptype

      | While (exp,stm) ->
	"while ( "  ^ exp_to_string exp ^ " ) " ^ stm_to_string stm psymbol ptype
	  
      | Comp (cstm) -> cstm_to_string cstm psymbol ptype
	
      | Expr (exp) -> exp_to_string exp  ^ " ;\n"
      | Skip ->  ""

and input_to_string = function
  | (lvalue,name) -> lvalue_to_string lvalue ^ " = " ^ name

and lvalue_to_string lvalue = match lvalue with
  | ValId(v) ->  v
  | ValComp(a,b) ->  a ^ "." ^ b

and receive_to_string = function
  | DontReceive -> ""
  | Receive(inputs) ->  " receive[ " ^ (string_list_separated inputs input_to_string  " , ")  ^ " ]"

and document_to_string = function
  | DocumentId(s) ->  s
  | Plug(name,plugs) -> " plug " ^ name ^ " [ " ^ (string_list_separated plugs plug_to_string " , ")  ^ " ] "

and plug_to_string = function
  | (name,exp) -> name ^ " = " ^ exp_to_string exp

and exp_to_string = function
  | Lval(v) -> lvalue_to_string  v
  | LvalAssign(l,exp) -> lvalue_to_string l ^ " = " ^ (exp_to_string exp)
  | Eq (a,b) -> exp_to_string a  ^ " == " ^ exp_to_string  b
  | Neq (a,b) -> exp_to_string a ^ " != " ^ exp_to_string b
  | Lt (a,b) -> exp_to_string a ^ " < " ^ exp_to_string b
  | Gt (a,b) -> exp_to_string a ^ " > " ^ exp_to_string b
  | Le (a,b) -> exp_to_string a ^  " <= " ^ exp_to_string b
  | Ge (a,b) -> exp_to_string a ^  " >= " ^ exp_to_string b
  | Not (exp) -> "! " ^ exp_to_string exp
  | Opposite (exp) -> "- " ^ exp_to_string exp
  | Plus(a,b) -> exp_to_string a ^ " + " ^ exp_to_string b
  | Minus (a,b) -> exp_to_string a ^ " - " ^ exp_to_string b
  | Mult (a,b) -> exp_to_string a ^ " * " ^ exp_to_string b
  | Div(a,b) -> exp_to_string a ^ " / " ^ exp_to_string b
  | Mod (a,b) -> exp_to_string a ^ " % " ^ exp_to_string b
  | And (a,b) -> exp_to_string a ^ " && " ^ exp_to_string b
  | Or (a,b) -> exp_to_string a ^ " || " ^ exp_to_string b
  | Join (a,b) -> exp_to_string a ^ " << " ^ exp_to_string b
  | Keep (exp,stringl) -> exp_to_string exp ^ " \\+  " ^ 
    if ( List.length stringl > 1 ) then
	(  "( " ^ (string_list_separated stringl (fun x -> x) " , " ) ^ " )"
	)
    else 
	 (string_list_separated stringl (fun x -> x) " , " )
      
  | Remove (exp,stringl) -> exp_to_string exp ^ " \\-  " ^
    if ( List.length stringl > 1 ) then
	(  "( " ^ (string_list_separated stringl (fun x -> x) " , " ) ^ " )"
	)
    else 
	(string_list_separated stringl (fun x -> x) " , " )
	  
   | Apply (str,expl) -> str ^ "( " ^
     ( string_list_separated expl exp_to_string " , ") ^ " ) " 
     
  | BoolConst(b) -> (string_of_bool b)
  | IntConst(i) -> (string_of_int i)
  | StringConst(s) -> s
  | TupleExp (fieldvalues) -> " tuple { " ^ ( string_list_separated fieldvalues fieldvalue_to_string " , ") ^ " } "

and fieldvalue_to_string = function
  |  (name,exp)  -> name ^ " = " ^  exp_to_string exp

and function_to_string f psymbol ptype = match f with
  | (typet, name, arguments , cstm, st) ->
    (if psymbol then (symboltable_to_string st) else "" ) ^ typet_to_string typet ^ "  " ^ name ^ " ( " ^ (string_list_separated arguments argument_to_string ", " )
    ^ " ) " ^ cstm_to_string cstm psymbol ptype

