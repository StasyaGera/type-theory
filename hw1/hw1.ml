open List;;
open String;;
open Ltype;;

type peano = Z | S of peano;;
(* type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;; *)

exception Neg_num;;

let rec peano_of_int x = 
	match x with
	| 0 -> Z
	| _ -> S (peano_of_int (x - 1));;

let rec int_of_peano p = 
	match p with
	| Z -> 0
	| S p1 -> 1 + (int_of_peano p1);;

let inc x = S x;;

let rec add x y = 
	match y with
	| Z -> x
	| S y1 -> S (add x y1);;

let dec x = 
	match x with
	| Z -> raise Neg_num
	| S x1 -> x1;;

let rec sub x y = 
	match y with
	| Z -> x
	| S y1 -> sub (dec x) y1;;

let rec mul x y = 
	match y with
	| Z -> Z
	| S y1 -> add (mul x y1) x;;

let less x y = 
	try
		ignore(sub x y); false
	with
	| Neg_num -> true;;

let rec div x y =
	if (less x y) then
		Z
	else
		let x1 = sub x y in
		match x1 with
		| Z -> S Z
		| _ -> S (div x1 y);;

let rec power x y = 
	match y with
	| Z -> S Z
	| S y1 -> mul (power x y1) x;;

(* ------------------------------------------------ *)

let rec mrev x res =
	match x with
	| [] -> res
	| h::t -> mrev t (h::res);;

let rev x = mrev x [];;

let rec merge a b res = 
	match a with
	| [] -> List.rev_append res b
	| a_h::a_t -> 
		match b with
		| [] -> List.rev_append res a
		| b_h::b_t -> 
			if (a_h < b_h) then
				merge a_t b (a_h::res)
			else
				merge b_t a (b_h::res);;

let rec cut_half a1 a2 =
	if (List.length a1 > (List.length a2 + 1)) then
		cut_half (tl a1) ((hd a1)::a2)
	else
		(a1, rev a2);;

let rec merge_sort x = 
	match x with
	| [] -> x
	| x0::[] -> x
	| _ -> 
		let x1, x2 = cut_half x [] in
		merge (merge_sort x1) (merge_sort x2) [];;

(* ------------------------------------------------ *)

let rec string_of_lambda x = 
	match x with
	| Var v -> v
	| App (l1, l2) -> "(" ^ string_of_lambda l1 ^ " " ^ string_of_lambda l2 ^ ")"
	| Abs (s, l) -> "(\\\\" ^ s ^ "." ^ string_of_lambda l ^ ")";;

let lambda_of_string x = Parser.main Lexer.token (Lexing.from_string (String.trim x));;
