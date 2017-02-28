open String;;
open Ltype;;

(*
type lambda = 
	| Var of string 
	| Abs of string * lambda 
	| App of lambda * lambda
;;
*)

let rec string_of_lambda x = 
	match x with
	| Var v -> v
	| App (l1, l2) -> "(" ^ string_of_lambda l1 ^ " " ^ string_of_lambda l2 ^ ")"
	| Abs (s, l) -> "(\\\\" ^ s ^ "." ^ string_of_lambda l ^ ")";;

let lambda_of_string x = 
	Parser.main Lexer.token (Lexing.from_string (String.trim x));;