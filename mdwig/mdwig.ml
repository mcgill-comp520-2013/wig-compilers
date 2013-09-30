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
open Parser
open Lexing
open Arg
open Symboltable
open Typecheck

  (* default arg values *)
let nw = ref false
let nst = ref false
let ntc = ref false
let ncg = ref false
let output = ref "out.rb"
let input = ref ""
let print_ast = ref false
let print_symboltable = ref false
let print_type = ref false
let check_pp = ref false

let usage = "Usage: " ^ Sys.argv.(0) ^ " [-o OUTPUT_FILE] WIG_FILE"

let speclist =
  [
    ( "-o"  , Arg.String ( fun s -> output := s ) , 
    "\n\t Set the output CGI file" ) ;
    ( "-nw"  , Arg.Unit ( fun () -> nw := true ) , 
    "\n\tEnd the compiler execution before the weeder phase" ) ;
    ( "-nst"  , Arg.Unit ( fun () -> nst := true ) ,
    "\n\tEnd the compiler execution before the symbol table phase" ) ;
    ( "-ntc"  , Arg.Unit ( fun () -> ntc := true ) ,
    "\n\tEnd the compiler execution before the type checking phase" ) ;
    ( "-ncg"  , Arg.Unit ( fun () -> ncg := true ) ,
    "\n\tEnd the compiler execution before the codegeneration phase" ) ;
    ( "-print-ast"  , Arg.Unit ( fun () -> print_ast := true) ,
    "\n\tPrint the AST on stdout" ) ;
    ( "-print-symboltable"  , Arg.Unit ( fun () -> print_symboltable := true ) ,
    "\n\tAdd symboltable information on AST (need -print-ast)" ) ;
    ( "-print-type"  , Arg.Unit ( fun () -> print_type := true ) ,
    "\n\tAdd type information on AST (need -print-ast)" ) ;
    ( "-check-pp"  , Arg.Unit ( fun () -> check_pp := true ) ,
    "\n\tThis mode check if parse(pp(parse(input))) == parse(input)" ) ;
    ( "-version"  , Arg.Unit 
    ( fun () -> print_string "MDWig version 0.1" ; print_newline () ; exit 0) ,
    "\n\tPrint information about the program" ) ;
  ]

    
let _ = 
  (* Read the arguments *)
  Arg.parse
    speclist
    (* Check if the file is available *)
    (fun x -> ( if !input = "" then input := x else (raise (Arg.Bad ("File already provided, this argument is useless : " ^ x)))))
    usage;


  let lexbuf = from_channel (if !input <> "" then (open_in !input) else stdin) in
  let cache =
    let l = ref [] in
    fun lexbuf ->
      match !l with
	| x::xs -> l := xs; x
	| [] -> match Lexer.token lexbuf with
	    | [] -> failwith "oops"
	    | x::xs -> l := xs; x in
  let ast = Parser.wig cache lexbuf in (
    print_string "[PARSING] Done\n" ;
    ( if (!nw) then
      ( if (!print_ast) then print_string (Prettyprint.print false false ast) )
      else
        (
          Weeder.weed ast ;
        let wast = ast in 
        print_string "[WEEDING] Done\n" ;
        (* check pp mode ?*)
        if (!check_pp) then
         ( let pp1 = (Prettyprint.print false false ast) in
              let lexbuf2 = (from_string pp1) in
              let ast2 = (Parser.wig cache lexbuf2) in
                (*let pp2 = (Prettyprint.print false false ast2) in
                  (* if (pp1 = pp2) then
                    (print_string " [VALIDATION] print(parser(print(parser(file)))) ==
                      print(parser(file)).'\n" ;
                      exit 0
                    )
                  else
                    failwith "[ERROR] print(parser(print(parser(file)))) !=
                      print(parser(file))." *)
                  *)
                  print_string " [CHECK PP] Ok, the compiler parses its own
                  pretty print\n"
         )
        else 
        if (!nst) then
          ( if (!print_ast) then print_string (Prettyprint.print false false wast) )
        else
          (
            let sast = Symboltable.create wast in 
            print_string "[SYMBOL TABLE] Construction done\n" ;
            Symboltable.check sast ;
            print_string "[SYMBOL TABLE] Analysis done\n" ;
            if (!ntc) then
              ( if (!print_ast) then print_string (Prettyprint.print !print_symboltable false sast) )
            else 
              (
                Typecheck.check sast ;
                print_string "[TYPE CHECKING] Done\n" ;
                if (not !ncg) then
                let gen = Gen.ruby sast in
                print_string "[CODEGEN] Done\n";
                let file = open_out !output in
                  output_string file gen ;
                  (close_out file) ;
                  print_string ("[EMIT] Done in file " ^ !output ^ "\n") ;
                
                ( if (!print_ast) then print_string (Prettyprint.print !print_symboltable !print_type sast) )
              )

           )
          )
        )
    )
