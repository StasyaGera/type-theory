open List;;

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