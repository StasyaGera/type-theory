(* 
type lambda = 
	| Var of string 
	| Abs of string * lambda 
	| App of lambda * lambda
 *)
open Ltype

val string_of_lambda: lambda -> string
val lambda_of_string: string -> lambda