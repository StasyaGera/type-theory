open Merge_sort;;

exception Input_error;;

print_string 
"Usage:
* this test is for 'int lists only
* enter a function to test, then you can enter arguments to evaluate on
* enter 'change' to choose another function
* function names: merge_sort, rev
* enter 'exit' to exit\n"
;;

let rec read_fun() =
	print_string "Enter function name\n";
	match read_line() with
		| "exit" -> exit 0
		| "change" -> read_fun()
		| "rev" -> Merge_sort.rev
		| "merge_sort" -> Merge_sort.merge_sort
		| _ ->  
			print_string "Unknown function, check your input\n";
			read_fun()
;;

let parse_args args =
	List.map int_of_string (Str.split (Str.regexp "[' ']+") args)
;;

let print_list lst = 
	List.iter (fun x -> print_int x; print_string " ") lst;
	print_newline()
;;

let rec main() =
	let f = read_fun() in

	while true do
		print_string "Enter list in format 'a0 a1 a2 ...'\n";
		let args = read_line() in

		match args with
		| "change" -> main()
		| "exit" -> exit 0
		| _ ->
			try 
				print_list (f (parse_args args))
			with 
				| Failure cause -> print_string "Something went wrong, check your input\n"
	done
;;

main();;