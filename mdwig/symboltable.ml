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

exception AlreadyExistSymbol of string
exception NotFoundSymbol of string
exception InvalidTuple

(* Utility functions *)
let rec find_symbol name st =
  match st with
  | Root -> 
  raise(NotFoundSymbol("No symbol named \"" ^ name))
  | Ht(htval,parent) -> 
    if Hashtbl.mem htval name then
      () (* Hashtbl.find htval name *)
    else
      find_symbol name parent
  
let rec get_var_type name st =
  match st with
  | Root -> 
  raise(NotFoundSymbol("No symbol named \"" ^ name))
  | Ht(htval,parent) -> 
    if Hashtbl.mem htval name then
      match Hashtbl.find htval name with
      | ST_Variable (t) -> t
      | _ -> failwith "Not a variable"
    else
      get_var_type name parent
  
let insert_unique table key value =
  if Hashtbl.mem table key
	then
	  raise(AlreadyExistSymbol("\"" ^ key ^ "\" has already been declared as a " ^
    (Prettyprint.htval_to_string value) ) )
	else
	  Hashtbl.add table key value
  
let rec create_st_list l parent apply = match l with
  | [] -> []
  | h::[] -> (apply h parent):: []
  | h::t -> (apply h parent) :: (create_st_list t parent apply)

(* Recursive symbol table creation *)
let rec create_st_function f parent = match f with
  | (typet, name, args, cstm, _) ->
    let htval = Hashtbl.create 5 in
        List.iter (fun (ty,name) -> ( insert_unique htval name (ST_Variable ty)  ) )
      args ;
      let st = Ht(htval,parent) in
      (typet, name, args, (create_st_cstm cstm st), st )


and create_st_cstm cstm parent = match cstm with
  | (vars,stml,_) ->
      let htval = Hashtbl.create 5 in
      List.iter (function (ty, vars2) ->
        List.iter (fun v -> ( insert_unique htval v (ST_Variable ty)  ) )
        vars2)
      vars ;
      let st = Ht(htval,parent) in
      (vars, (create_st_list stml st create_st_stm) ,st )

and create_st_stm stm parent = match stm with
  | Comp cstm -> Comp( create_st_cstm cstm parent)
  | While ( exp , stm ) -> While(exp,(create_st_stm stm parent))
  | If ( exp , stm) -> If(exp, (create_st_stm stm parent))
  | IfElse (exp, stm1 , stm2) -> IfElse( exp, (create_st_stm stm1 parent) , (create_st_stm stm2 parent)) 
  | _ -> stm

and create_st_session session parent = match session with
  | (name,cstm) -> (name, (create_st_cstm cstm parent) )

let attr_receive = function
|WigAttr(name) -> name::[]
|_ -> []

let attribute_receive = function
|KeyValue(attr1,attr2) -> (attr_receive attr1) @ (attr_receive attr2) 
|Single(attr) -> attr_receive attr

let rec html_receive_list = function
| [] -> []
| h::t -> (match h with
	   |StartTagIdent (name, attrl) -> ( List.fold_left (fun init x -> init @ attribute_receive x) [] attrl)
	   |WigIdent(name) -> name::[]
	   |_ -> []
	) @ (html_receive_list t)

let rec html_send_list = function
| [] -> []
| h::t -> (match h with
	    | Input (inputattrl) -> (match (List.find
					(fun x -> match x with 
						| Name (_) -> true 
						| _ -> false
					)
					inputattrl
				    ) with
				    | Name(x) -> x
				    | _ -> raise(InternalError)
				    )
				    ::[]
	   |_ -> []
	) @ (html_send_list t)

(* Recursive symbol table for wig *)
let create = function
  | Lang.Wig(htmls,schemas,vars,functions,sessions,_) ->
      let htval = Hashtbl.create 5 in
      (* Symbol table for wig variables *)
      List.iter (function (ty, vars2) ->
        List.iter (fun v -> insert_unique htval v (ST_Variable ty) ) 
        vars2)
      vars ;
      (* Symbol table for wig html *)
      List.iter (function (name, content) -> ( insert_unique htval name
      (ST_Html( (html_receive_list content), (html_send_list content))) ))
      htmls ;
      (* Symbol table for wig function *)
      List.iter (function (typet, name, args, cstm, _) -> ( insert_unique htval
      name (ST_Func(typet, (List.map (fun (x,y) -> x) args) ))))
      functions ;
      (* Symbol table for wig function *)
      List.iter (function (name, fields) -> ( insert_unique htval name (ST_Schema fields)  ) )
      schemas;
      let st = Ht(htval , Root) in
      Wig(htmls,schemas,vars, ( create_st_list functions st create_st_function) ,(create_st_list sessions st create_st_session),st)



 (* Recursive symbol analysis *)
let rec check = function
  | Lang.Wig(htmls,schemas,vars,functions,sessions,st) ->
      List.iter (function (typet, name, args, cstm, stf) -> (check_cstm cstm stf) )
      functions ;
      List.iter (function (name,cstm) -> (check_cstm cstm st) )
      sessions ;

      
and check_cstm cstm st = match cstm with
  | (vars,stml,st) ->
      List.iter (function (stm) -> (check_var stm st) )
      vars ;
      List.iter (function (stm) -> (check_stm stm st) )
      stml ;

and check_var var st = match var with
| (typet, stringl) -> (match typet with
			| TupleType(tuple) -> ( match tuple with 
							| Static name -> find_symbol name st
							| Dynamic fieldl -> raise(InvalidTuple)
						    )
			| SimpleType(t) -> () 
			)

and check_stm stm st = match stm with
  | Comp cstm -> (check_cstm cstm st)
  | While (exp,stm) -> (check_stm stm st)
  | If (exp,stm) -> (check_stm stm st)
  | IfElse (exp,stm1,stm2) -> (check_stm stm1 st) ; (check_stm stm2 st)
  | Skip -> ()
  | ReturnVoid -> ()
  | Return(exp) -> (check_exp exp st)
  | Expr(exp) -> (check_exp exp st)
  | Show(document,receive) -> ()
  | Exit (document) -> ()

and check_exp exp st = match exp with
  | Lval (lvalue) -> check_lvalue lvalue st
  | LvalAssign (lvalue,exp) -> (check_lvalue lvalue st) ; (check_exp exp st)
  | Eq (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Neq (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Lt (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Gt (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Le (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Ge (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Not (exp) -> (check_exp exp st)
  | Opposite(exp) -> (check_exp exp st)
  | Plus (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Minus (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Div (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Mod (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | And (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Or (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | Join (exp1, exp2) -> (check_exp exp1 st) ; (check_exp exp2 st)
  | _ -> ()
    
and check_lvalue lvalue st = match lvalue with
  |ValId(name) -> find_symbol name st
  |ValComp(left,right) -> find_symbol left st (* the right can be anything, the typecheking system handle that *)
