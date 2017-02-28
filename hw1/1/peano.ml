type peano = Z | S of peano;;

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
