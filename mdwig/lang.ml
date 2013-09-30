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


exception InternalError

(* Definition of source language data structures *)
type simpletype =
  | Int
  | Bool
  | String
  | Void

type typet =
  | SimpleType of simpletype
  | TupleType of tuple
and tuple =
    | Static of string
    | Dynamic of (field list)
and field = simpletype * string


type argument = typet * string


type tagname = string
type wigname = string
type schemaname = string
type attr =
  | Identifier of string
  | Stringconst of string
  | Intconst of int
  | WigAttr of wigname

type htval = 
  | ST_Html of (string list) * (string list) (* receive / send *)
  | ST_Func of typet * (typet list)
  | ST_Schema of (field list)
  | ST_Variable of typet

type htkey = string
type ht = (htkey , htval) Hashtbl.t
type symboltable =
  | Ht of ht * symboltable
  | Root

 (* Grammar *)
type attribute = 
  | KeyValue of attr * attr
  | Single of attr
type whatever = string
type meta = string
type inputtype =
  | Radio
  | Text
type inputattr =
  | Name of string
  | Type of inputtype
  | Attribute of attribute
type htmlbody = 
  | StartTagIdent of tagname * (attribute list)
  | EndTagIdent of tagname
  | WigIdent of  wigname
  | Whatever of whatever
  | Meta of meta
  | Input of (inputattr list)
type htmlname = string
type html = htmlname * (htmlbody list)
type schema = schemaname * (field list)
type variable = typet * (string list)
type lvalue = 
  | ValId of string
  | ValComp of string * string
type fieldvalue = string * exp
and
exp =
  | Lval of lvalue
  | LvalAssign of lvalue *  exp
  | Eq of exp * exp
  | Neq of exp * exp
  | Lt of exp * exp
  | Gt of exp * exp
  | Le of exp * exp
  | Ge of exp * exp
  | Not of exp
  | Opposite of exp
  | Plus of exp * exp
  | Minus of exp * exp
  | Mult of exp * exp
  | Div of exp * exp
  | Mod of exp * exp
  | And of exp * exp
  | Or of exp * exp
  | Join of exp * exp
  | Keep of exp * (string list)
  | Remove of exp * (string list)
  | Apply of string * (exp list)
  | IntConst of int
  | BoolConst of bool
  | StringConst of string
  | TupleExp of (fieldvalue list)
type input = lvalue * string
type plug = string * exp 
type document = 
  | DocumentId of string
  | Plug of string * (plug list)
type receive =
  | DontReceive
  | Receive of (input list) 
type cstm =  (variable list) * (stm list) * symboltable
and
 stm = 
  | Skip
  | Show of document * receive
  | Exit of document
  | ReturnVoid
  | Return of exp
  | If of exp * stm
  | IfElse of exp * stm * stm
  | While of exp * stm
  | Comp of cstm
  | Expr of exp
type functiont = typet * string * (argument list) * cstm * symboltable
type session = string * cstm
type wig = Wig of (html list) * (schema list) * (variable list) * (functiont
list) * (session list) * symboltable
