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

(* Simple return checking weeder, 
 * which would be very hard to verify directly into the
 * grammar *)

exception NonReturnBlock of string 

type wigreturn =
  | ExitReturn
  | TypeReturn 

let rec return_stm stm funs wigr = match stm with
  | Comp cstm -> return_cstm cstm funs wigr
  | Show (document , receive ) -> false
  | Exit ( document ) -> (match wigr with
			    | ExitReturn -> true
			    | TypeReturn -> false
			)
  | While ( exp , stm ) -> return_stm stm funs wigr
  | If (exp,stm) -> return_stm stm funs wigr
  | IfElse (exp,stm1,stm2) ->
      return_stm stm1 funs wigr && return_stm stm2 funs wigr
  | Skip -> false
  | ReturnVoid -> (match wigr with
			    | ExitReturn -> false
			    | TypeReturn -> true
		)
  | Return ( exp) -> (match wigr with
			    | ExitReturn -> false
			    | TypeReturn -> true
		)
  | Expr (Apply (func, _)) ->
      (match wigr with
             | ExitReturn ->
                 (match List.find (fun (_, name, _, _, _) -> name = func) funs with
		 (_, _, _, cstm, _) -> return_cstm cstm funs ExitReturn)
             | _ -> false
      )
  | Expr (exp) -> false

and return_stml stml funs wigr = match stml with
| [] -> false
| h::t -> if return_stm h funs wigr then true else return_stml t funs wigr

and return_cstm cstm funs wigr = match cstm with
  | (varl,stml,st) -> (return_stml stml funs wigr)
	    

let weed ast = match ast with
  | Lang.Wig(htmls,schemas,variables,functions,sessions,st) ->
      List.iter
      (fun (typet,name,args,cstm,st) -> 
      match typet with
      | SimpleType(Void) -> ()
      | _ -> if not (return_cstm cstm functions TypeReturn) then 
	     raise (NonReturnBlock("Function "^ name ^ " doesn't always return")))
      functions ;

      List.iter
      (fun (name,cstm) -> 
	  if not (return_cstm cstm functions ExitReturn) then 
	    raise (NonReturnBlock("Session "^ name ^ " doesn't always return")))
      sessions


