open Ltype;;

let rec get_fresh l = 
	match l with
	| Var x -> x ^ x
	| App (p, q) -> (get_fresh p) ^ (get_fresh q)
	| Abs (x, p) -> x ^ (get_fresh p)

let rec subst l a b =
	match l with
	| Var x -> 
		if (x = a) then Var b
		else l
	| App (p, q) -> App ((subst p a b), (subst q a b))
	| Abs (x, p) ->
		if (x = a) then l
		else Abs (x, (subst p a b))

let rec alpha_eq l1 l2 =
	match l1, l2 with
		| Var x1, Var x2 -> x1 = x2
		| App (p1, q1), App (p2, q2) -> (alpha_eq p1 p2) && (alpha_eq q1 q2)
		| Abs (x1, p1), Abs (x2, p2) -> 
			let t = (get_fresh p1) ^ (get_fresh p2) in
			alpha_eq (subst p1 x1 t) (subst p2 x2 t)
		| _ -> false