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

let id = ref 1
let funs = ref []
let symtbl = ref Root

let rec string_list_separated l to_string_fun separation =
match l with
  | [] -> ""
  | h::[] -> (to_string_fun h)
  | h::t -> (to_string_fun h) ^ separation ^ ( string_list_separated t to_string_fun separation)

and argument_to_string = function
    | (typet,name) -> name

and field_to_string = function
    | (a,b) -> b

and fields_to_string x =
    List.fold_left
       ( fun init field -> init ^ (field_to_string field) )
       ""
       x

let rec ruby wig =  match wig with
  | Wig(htmls,schemas,variables,functions, sessions, st) ->
      symtbl := st ;
      let res = "#! /usr/bin/env ruby\n\nrequire 'cgi'\nrequire 'cgi/session'\n\n" ^
      "$cgi = CGI.new('html4')\n$sess = CGI::Session.new ($cgi)\n" ^
      "if !$sess[\"__return\"] then $sess[\"__return\"] = [] end" ^
  (List.fold_left ( fun x y -> x ^ "\n" ^ (variable_init true y)) "" variables) ^
  (List.fold_left ( fun x y -> x ^ "\n" ^ (html_to_string y))  "" htmls ) ^
  (List.fold_left ( fun x y ->  x ^ "\n" ^(function_to_string y) ) ""  functions ) ^
  (List.fold_left ( fun x y ->  x ^ "\n" ^(session_to_string y) )  "" sessions ) in
  res ^ (List.fold_left (^) "" !funs ) ^
  "if !$sess[\"__fun\"]\n" ^
  (match sessions with
  | [] -> failwith "No session"
  | [(sess, _)] -> "session" ^ sess
  | _ ->
      "if $cgi[\"__page\"].empty?\n$cgi.out { \"<html><body><ul>" ^
      List.fold_left (fun res -> function (sess, _) -> res ^ "<li><a href=\\\"?__page=" ^ sess ^ "\\\">" ^ sess ^ "</a></li>") "" sessions
      ^ "</ul></body></html>\" }\nelse\nsend (\"session#{$cgi[\"__page\"]}\")\nend\n"
  ) ^
  "\nelse\nsend (\"fun#{$sess[\"__fun\"]}\")\nend\n"

and html_to_string = function
  | (name,bodies) ->
   "def html" ^ name ^ " params\n  \"<form>" ^
    (List.fold_left
       (fun init x -> init ^ (htmlbody_to_string x) )
       ""
       bodies
    )
    ^ "<input type=\\\"submit\\\" /></form>\"\nend"
      
and htmlbody_to_string = function
  | StartTagIdent(name,attrs)  -> if name = "body" then "" else "<" ^ name ^ " " ^ (string_list_separated attrs attribute_to_string " " ) ^ ">"
  | EndTagIdent(name) -> if name = "body" then "" else "</" ^ name ^ ">"
  | WigIdent(name) -> "#{params[\"" ^ name ^ "\"]}"
  | Whatever(content) -> Str.global_replace (Str.regexp_string "\"") "\\\"" content
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
  | Name(a) -> "name=" ^ Str.global_replace (Str.regexp "\"") "\\\"" a
  | Type(t) -> "type=" ^ ( match t with 
                                          | Radio -> "\\\"radio\\\""
                                          | Text -> "\\\"text\\\"" )
  | Attribute(x) -> attribute_to_string x

    
and attr_to_string = function
  | Identifier(i) -> i
  | Stringconst(s) -> Str.global_replace (Str.regexp "\"") "\\\"" s
  | Intconst(i) -> string_of_int i
  | WigAttr(s) -> "#{params[\"" ^ s ^ "\"]}"

and session_to_string s = match s with
  | (name,cstm) -> "def session" ^ name ^ (cstm_to_string cstm) ^ "\nend\n"

and cstm_to_string cstm = match cstm with
  | (varl,stml,st) ->
    symtbl := st ;
    "\n" ^
    (List.fold_left (fun x y -> x ^ variable_init false y) "" varl) ^
    (List.fold_left
       ( fun init y -> init ^ "\n" ^(stm_to_string y ) )
       ""
       stml
    )

and add_function id stm =
    funs := ("def fun" ^ string_of_int id ^ "\n" ^ stm ^ "\nend\n")::!funs

and stm_to_string stm = match stm with
      | Show (document,receive) ->
	(document_to_string document) ^ " " ^ (receive_to_string receive) ^ "\n"
      | Exit (document) ->
        (document_to_string document) ^ "\n$sess[\"__fun\"] = nil\nexit 0\n"
      | ReturnVoid ->
	"return\n"
      | Return(exp) ->
	"return " ^ (exp_to_string exp) ^ "\n"
      | If (exp,stm) ->
        let curid = !id in
	id := !id + 2 ;
	add_function curid ((stm_to_string stm) ^ "\nfun" ^ string_of_int (curid + 1)) ;
	"if " ^ (exp_to_string exp)  ^ "\nfun" ^ string_of_int curid ^ "\nelse\nfun" ^ string_of_int (curid + 1) ^ "\nend\n" ^
	"\nend\ndef fun" ^ string_of_int (curid + 1) ^ "\n"
	  
      | IfElse (exp,stm1,stm2) ->
        let curid = !id in
	id := !id + 3 ;
	add_function curid (stm_to_string stm1 ^ "\nfun" ^ string_of_int (curid + 2)) ;
	add_function (curid + 1) (stm_to_string stm2 ^ "\nfun" ^ string_of_int (curid + 2)) ;
	"if " ^ (exp_to_string exp) ^ "\nfun" ^ string_of_int curid ^ "\nelse\nfun" ^ string_of_int (curid + 1) ^ "\nend\n" ^
	"end\ndef fun" ^ string_of_int (curid + 2) ^ "\n"

      | While (exp,stm) ->
        let curid = !id in
	id := !id + 2 ;
        let cond = "# condition of while\nif "  ^ exp_to_string exp ^ "\nfun" ^ string_of_int curid ^ "\nelse\nfun" ^ string_of_int (curid + 1) ^ "\nend\n" in
	add_function curid (stm_to_string stm ^ "\n" ^ cond) ;
	cond ^ "end\ndef fun" ^ string_of_int (curid + 1) ^ "\n"
	  
      | Comp (cstm) -> cstm_to_string cstm
	
      | Expr (exp) ->
          (match exp with
	  | Apply (str, expl) ->
              let curid = !id in
	      id := !id + 1 ;
	      "$sess[\"__return\"].push \"" ^ string_of_int curid ^ "\"\n" ^
	      "fun" ^ str ^ " " ^
              ( string_list_separated expl exp_to_string " , ") ^
	      "\nend\ndef fun" ^ string_of_int curid ^ "\n"
          | _ -> exp_to_string exp ^ "\n")
      | Skip ->  ""

and input_to_string = function
  | (lvalue,name) -> lvalue_to_string lvalue ^ " = " ^ name

and lvalue_to_string lvalue = match lvalue with
  | ValId(v) -> "$sess[\"" ^ v ^ "\"]"
  | ValComp(a,b) -> "$sess[\"" ^ a ^ "\"][\"" ^ b ^ "\"]"

and receive_to_string r =
  id := !id + 1 ;
  "\n$sess[\"__fun\"] = " ^ string_of_int (!id - 1) ^ "\nexit 0\nend\ndef fun" ^ string_of_int (!id - 1) ^ "\n" ^
  match r with
  | DontReceive -> ""
  | Receive(inputs) ->
      List.fold_left (fun res -> function (var, input) ->
        res ^ (lvalue_to_string var) ^ " = $cgi[\"" ^ input ^ "\"]" ^
        (match Typecheck.type_lvalue var !symtbl with
	 | SimpleType(Int) -> ".to_i" | _ -> "") ^ "\n"
	) "" inputs

and document_to_string = function
  | DocumentId(s) -> "$cgi.out { html" ^ s ^ " [] }"
  | Plug(name,plugs) -> "$cgi.out { html" ^ name ^ " " ^ (string_list_separated plugs plug_to_string " , ")  ^ " }"

and plug_to_string = function
  | (name,exp) -> "\"" ^ name ^ "\" => " ^ exp_to_string exp

and exp_to_string = function
  | Lval(v) -> lvalue_to_string  v
  | LvalAssign(l,exp) -> lvalue_to_string l ^ " = " ^ (exp_to_string exp)
  | Eq (a,b) -> exp_to_string a ^ " == " ^ exp_to_string b
  | Neq (a,b) -> exp_to_string a ^ " != " ^ exp_to_string b
  | Lt (a,b) -> exp_to_string a ^ " < " ^ exp_to_string b
  | Gt (a,b) -> exp_to_string a ^ " > " ^ exp_to_string b
  | Le (a,b) -> exp_to_string a ^  " <= " ^ exp_to_string b
  | Ge (a,b) -> exp_to_string a ^  " >= " ^ exp_to_string b
  | Not (exp) -> "! (" ^ exp_to_string exp ^ ")"
  | Opposite (exp) -> "- " ^ exp_to_string exp
  | Plus(a,b) -> exp_to_string a ^ " + " ^ exp_to_string b
  | Minus (a,b) -> exp_to_string a ^ " - " ^ exp_to_string b
  | Mult (a,b) -> exp_to_string a ^ " * (" ^ exp_to_string b ^ ")"
  | Div(a,b) -> exp_to_string a ^ " / (" ^ exp_to_string b ^ ")"
  | Mod (a,b) -> exp_to_string a ^ " % (" ^ exp_to_string b ^ ")"
  | And (a,b) -> exp_to_string a ^ " && " ^ exp_to_string b
  | Or (a,b) -> exp_to_string a ^ " || " ^ exp_to_string b
  | Join (a,b) ->
    let va = exp_to_string a and vb = exp_to_string b in
    (match Typecheck.type_exp a !symtbl, Typecheck.type_exp b !symtbl with
    | TupleType(Dynamic(fa)), TupleType(Dynamic(fb)) ->
      "{" ^ string_list_separated fa (function t, x -> "\"" ^ x ^ "\" => " ^ va ^ "[\"" ^ x ^ "\"]") ", " ^
      (match fb with
      | [] -> ""
      | _::_ -> ", " ^ string_list_separated fb (function t, x -> "\"" ^ x ^ "\" => " ^ vb ^ "[\"" ^ x ^ "\"]") ", " ^ "}")
    | _ -> failwith ("Using join operator on non-tuple expression."))
  | Keep (exp,stringl) ->
    let v = exp_to_string exp in
    "{" ^ string_list_separated stringl (fun x -> "\"" ^ x ^ "\" => " ^ v ^ "[\"" ^ x ^ "\"]") ", " ^ "}"
  | Remove (exp,stringl) ->
    (match Typecheck.type_exp exp !symtbl with
    | TupleType(Dynamic(fields)) -> 
      exp_to_string (Keep (exp,
          List.fold_left (fun r -> function t, name ->
               if List.exists (fun n -> name = n) stringl then r else r@[name]
	  ) [] fields))
    | _ -> failwith ("Using keep operator on non-tuple expression."))
  | Apply (str,expl) -> "fun" ^ str ^ " " ^
    ( string_list_separated expl exp_to_string " , ") ^ " " 
  | BoolConst(b) -> (string_of_bool b)
  | IntConst(i) -> (string_of_int i)
  | StringConst(s) -> s
  | TupleExp (fieldvalues) -> " { " ^ ( string_list_separated fieldvalues fieldvalue_to_string " , ") ^ " } "

and fieldvalue_to_string = function
  | (name,exp) -> "\"" ^ name ^ "\" => " ^  exp_to_string exp

and function_to_string f = match f with
  | (typet, name, arguments , cstm, st) ->
    "def fun" ^ name ^ " " ^ (string_list_separated arguments argument_to_string ", " )
    ^ "\n" ^ cstm_to_string cstm ^ "\nsend (\"fun#{$sess[\"__return\"].pop}\")\nend\n"

and variable_init = fun guard -> function
  | (typet, names) ->
      let value = default_value typet in
      List.fold_left (fun x y ->
        x ^ (if guard then "if $sess[\"" ^ y ^ "\"].nil? then" else "") ^ "$sess[\"" ^ y ^ "\"] = " ^ value ^ (if guard then " end\n" else "\n")
      ) "" names

and default_value = function
  | SimpleType(Int) -> "0"
  | SimpleType(Bool) -> "false"
  | SimpleType(String) -> "\"\""
  | SimpleType(Void) -> failwith "can get default value of void"
  | TupleType(Dynamic(fields)) ->
      "{" ^ string_list_separated fields (function t, name -> "\"" ^ name ^ "\" => " ^ default_value (SimpleType t)) ", " ^ "}"
  | TupleType(Static(name)) ->
      match Typecheck.find_symbol name !symtbl with
      | ST_Schema(fields) -> default_value (TupleType (Dynamic fields))
      | _ -> failwith ("Schema " ^ name ^ " undefined")
