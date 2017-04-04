open Ltype;;
open List;;

let rec get_free expr free except = 
	match expr with
	| Var x -> 
		if (not (List.mem x free) && not (List.mem x except)) then (x :: free) 
		else free
	| App (p, q) -> get_free p (get_free q free except) except
	| Abs (x, p) -> get_free p free (x :: except)
;;

let rec free_subst theta expr var = 
	match expr with
	| App (p, q) -> (free_subst theta p var) && (free_subst theta q var)
	| Abs (x, p) -> 
		if (x == var) then true
		else if (not (List.mem x (get_free theta [] [])) && (free_subst theta p var)) then true
		else false
	| _ -> true
;;
