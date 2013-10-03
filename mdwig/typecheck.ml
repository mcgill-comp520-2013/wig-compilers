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
open Prettyprint

exception InternalTypingError
exception InvalidType of string
exception InvalidAssign of string
exception NotAFunction
exception NotAVariable
exception NotATuple
exception NotASchema
exception NotADocument
exception NoRecursiveTuple
exception InvalidFunctionArgumentNumber
exception InvalidDocumentArgumentNumber of string
exception InvalidFunctionArgumentType
exception TupleTypeError of string

type returntype = 
| ExitReturn 
| ClassicReturn of typet

let rec find_symbol name st =
  match st with
  | Root ->  raise(InternalTypingError)
  | Ht(htval,parent) -> 
    if Hashtbl.mem htval name then
       Hashtbl.find htval name
    else
      find_symbol name parent

(* Type checking *)
let rec type_lvalue lvalue st = match lvalue with
  | ValId(name) ->  (match find_symbol name st with
                      | ST_Variable(t) -> ( match t with
			  | TupleType(Static(n)) -> 
			      ( match find_symbol n st with
				  | ST_Schema(fl) -> TupleType(Dynamic(fl))
				  | _ -> raise(NotASchema)
			      )
			  | _ -> t
			)
                      | _ -> raise(NotAVariable)
                      )

  | ValComp(left,right) ->
                     ( match find_symbol left st with
                      | ST_Variable(t) -> ( match t with
					    | TupleType(tt) ->
						( match tt with
						    | Static n -> (* It's a schema staticly declared *)
							( match find_symbol n st with
							    | ST_Schema(fl) -> find_right_type fl right
							    | _ -> raise(NotASchema)
							)
						    | Dynamic fieldl -> find_right_type fieldl right
						)
					    | SimpleType(x) -> raise(NotATuple)
					    )
                      | _ -> raise(NotAVariable)
                     )

and find_right_type fieldl right = 
SimpleType( fst 
		     (List.find
			 ( fun (simpletype,name) -> name = right )
			 fieldl
		     )
	    )

let rec fmtb exp1 exp2 st = 
    (* from multiple to boolean - string/int ..*)
      let type1 = (type_exp exp1 st) in
        let type2 = (type_exp exp2 st) in
         if type1 <> type2 then
            raise(InvalidType("The types doesn't match : [" ^  (typet_to_string type2) ^ "] and [" ^ (typet_to_string type1) ^ "]" ) )
	else
	    SimpleType(Bool)

and fmtm exp1 exp2 st = 
    (* from multiple to multiple - string/int ..*)
      let type1 = (type_exp exp1 st) in
        let type2 = (type_exp exp2 st) in
         if type1 <> type2 then
            raise(InvalidType("The types doesn't match : [" ^  (typet_to_string type2) ^ "] and [" ^ (typet_to_string type1) ^ "]" ) )
	else
	    type1

and fitb exp1 exp2 st =
  (* From Integer To Boolean *)
      let type1 = (type_exp exp1 st) in
        let type2 = (type_exp exp2 st) in
         if type1 <> SimpleType(Int) then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Int))) ^ "\n Received : " ^ (typet_to_string type1) ) )
	else
	    if type2 <> SimpleType(Int) then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Int))) ^ "\n Received : " ^ (typet_to_string type2) ) )
	    else
            SimpleType(Bool)

and fiti exp1 exp2 st =
  (* From Integer To Integer *)
      let type1 = (type_exp exp1 st) in
        let type2 = (type_exp exp2 st) in
         if type1 <> SimpleType(Int) then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Int))) ^ "\n Received : " ^ (typet_to_string type1) ) )
	else
	    if type2 <> SimpleType(Int) then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Int))) ^ "\n Received : " ^ (typet_to_string type2) ) )
	    else
            SimpleType(Int)

and fbtb exp1 exp2 st =
  (* From Boolean To Boolean *)
      let type1 = (type_exp exp1 st) in
        let type2 = (type_exp exp2 st) in
         if type1 <> SimpleType(Bool) then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Bool))) ^ "\n Received : " ^ (typet_to_string type1) ) )
	else
	    if type2 <> SimpleType(Bool) then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Bool))) ^ "\n Received : " ^ (typet_to_string type2) ) )
	    else
            SimpleType(Bool)

and resolve_schema ret st = match ret with
  | TupleType(Static(n)) -> 
					    ( match find_symbol n st with
						| ST_Schema(fl) -> TupleType(Dynamic(fl))
						| _ -> raise(NotASchema)
					    )
					| _ -> ret
          
and type_exp exp st = match exp with
  | Lval (lvalue) -> type_lvalue lvalue st
  | LvalAssign (lvalue,exp) -> let type1 = type_lvalue lvalue st in
        let type2 = type_exp exp st in
            if type1 <> type2
            then
                raise(InvalidAssign("LValue of type " ^ typet_to_string type1 ^ " can't receive a value of type " ^ typet_to_string type2 ))
            else
                type1
            


  | Eq (exp1, exp2) -> fmtb exp1 exp2 st
  | Neq (exp1, exp2) -> fmtb exp1 exp2 st
  | Lt (exp1, exp2) -> fitb exp1 exp2 st
  | Gt (exp1, exp2) -> fitb exp1 exp2 st
  | Le (exp1, exp2) -> fitb exp1 exp2 st
  | Ge (exp1, exp2) -> fitb exp1 exp2 st


  | Not (exp) -> let t = type_exp exp st in
    if t <> SimpleType(Bool)
    then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Bool))) ^ "\n Received : " ^ (typet_to_string t) ) )
    else t
  | Opposite(exp) -> let t = type_exp exp st in
    if t <> SimpleType(Int)
    then
            raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Int))) ^ "\n Received : " ^ (typet_to_string t) ) )
    else t


  | Plus (exp1, exp2) -> fmtm exp1 exp2  st
  | Minus (exp1, exp2) -> fiti exp1 exp2  st
  | Div (exp1, exp2) -> fiti exp1 exp2  st
  | Mult (exp1,exp2) -> fiti exp1 exp2  st
  | Mod (exp1, exp2) -> fiti exp1 exp2  st


  | And (exp1, exp2) -> fbtb exp1 exp2  st
  | Or (exp1, exp2) -> fbtb exp1 exp2 st


  | Join (exp1, exp2) -> ( match (type_exp exp1 st) with
		    | TupleType(Dynamic(fieldl1)) -> 
			TupleType(Dynamic(fieldl1 @
			    (match (type_exp exp2 st) with 
				| TupleType(Dynamic(fieldl2)) ->
				    List.fold_left
				    (fun init x -> []@(if List.mem x fieldl1 then [] else x::[]))
				    []
				    fieldl2
				| _ -> raise(NotATuple)
			    )
			    
			))
		    | _ -> raise(NotATuple) 
		)

  | Keep (exp,stringl) -> (match type_exp exp st with
		| TupleType(Dynamic(fieldl)) -> TupleType(Dynamic( 
		List.map
		( fun name -> ( fst (try (List.find (fun (st,n) -> n = name ) fieldl) with 
				    | e -> raise(TupleTypeError("Tuple member '" ^ name ^ "' not found")))
				,name
		))
		stringl
		))
		| _ -> raise(NotATuple) 
			    )

  | Remove (exp,stringl) -> (match type_exp exp st with
		| TupleType(Dynamic(fieldl)) -> TupleType(Dynamic( 
		let rec construct_tuple_list stringl fieldl  =
		    match fieldl with
			| [] -> [] 
			| (st,name)::t -> if List.mem name stringl then
					(construct_tuple_list stringl t)
					else (st,name)::(construct_tuple_list stringl t)
		in (construct_tuple_list stringl fieldl)
		)) (* end of tupletype *)
		| _ -> raise(NotATuple) 
			    )

  | Apply (str,expl) -> ( match find_symbol str st with
			    | ST_Func(ret,argl) -> 
				if (List.length expl <> List.length argl) then
				    raise(InvalidFunctionArgumentNumber)
				else
				    List.iter2 
					(fun x y -> if resolve_schema (type_exp x st) st <> resolve_schema y st then 
                                            print_string "error applying type " ;
                                            (print_string (typet_to_string (resolve_schema (type_exp x st) st))) ;
                                            print_string " in expression " ;
                                            (print_string (exp_to_string exp)) ;
                                            print_string " and type " ;
                                            (print_string (typet_to_string (resolve_schema y st))) ;
                                            raise(InvalidFunctionArgumentType))
					expl
          argl
					;
				    ( match ret with 
					| TupleType(Static(n)) -> 
					    ( match find_symbol n st with
						| ST_Schema(fl) -> TupleType(Dynamic(fl))
						| _ -> raise(NotASchema)
					    )
					| _ -> ret
				    )
			    | _ -> raise(NotAFunction)
			)

  | BoolConst(b) -> SimpleType(Bool)
  | IntConst(i) -> SimpleType(Int)
  | StringConst(s) -> SimpleType(String)

  | TupleExp (fieldvaluel) -> TupleType(Dynamic(
				   List.map 
				   ( fun (name,exp) -> let texp = type_exp exp st in (
				    (match texp with 
					| SimpleType x -> x
					| TupleType y -> raise(NoRecursiveTuple))
				    ,name) )
				   fieldvaluel
			    )) 

let rec check_cstm = function
  | (varl,stml,st) ->
    List.iter
      (fun x -> check_stm x st)
      stml


and check_document document receive st = match document with
    | DocumentId(name) ->  (match find_symbol name st with
				| ST_Html(receive,send) -> 
				    let rsize = List.length receive in
				    if rsize <> 0 
					then raise(InvalidDocumentArgumentNumber
					    ("The HTML " ^ name ^ "expects " ^ (string_of_int rsize)^ " parameters but here has 0") )
				| _ -> raise(NotADocument)
			    )

    | Plug(name,plugl) -> 
	(match find_symbol name st with
	    | ST_Html(receive,send) -> 
		let rsize = List.length receive in
		let argsize = List.length plugl in
		if rsize <> argsize 
		then 
		raise(InvalidDocumentArgumentNumber
			("The HTML " ^ name ^ "expects " ^ (string_of_int rsize) ^ " parameters but here has " ^ (string_of_int argsize) ) )
		else
		(List.iter
		    ( fun (name,exp) -> if List.mem name receive then () else raise(NotATuple) )
		 plugl)

		| _ -> raise(NotADocument)
	    )



and check_stm stm st = match stm with
  | Comp cstm -> check_cstm cstm
  | Show (document , receive ) -> check_document document receive st
  | Exit ( document ) -> check_document document DontReceive st
  | While ( exp , stm ) ->
      let t = (type_exp exp st) in
      if t <> SimpleType(Bool) then
	  raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Bool))) ^ "\n Received : " ^ (typet_to_string t) ) )
	else
      check_stm stm st

  | If (exp,stm) ->
      let t = (type_exp exp st) in
      if t <> SimpleType(Bool) then
	  raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Bool))) ^ "\n Received : " ^ (typet_to_string t) ) )
	else
      check_stm stm st

  | IfElse (exp,stm1,stm2) ->
      let t = (type_exp exp st) in
      if t <> SimpleType(Bool) then
	  raise(InvalidType("Excepted : " ^  (typet_to_string (SimpleType(Bool))) ^ "\n Received : " ^ (typet_to_string t) ) )
	else
      check_stm stm1 st ;
      check_stm stm2 st

  | Skip -> ()
  | ReturnVoid -> ()
  | Return ( exp) ->  ()
  | Expr (exp) -> match type_exp exp st with _ -> () (* hack *)

let check = function
  | Lang.Wig(htmls,schemas,vars,functions,sessions,st) ->
      List.iter
        ( function (typet,name,args,cstm,st) -> check_cstm cstm) 
        functions ;
      List.iter 
         ( function (name,cstm) -> check_cstm cstm)
         sessions
      
