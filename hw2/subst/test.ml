open Ltype;;
open Parse_lambda;;
open Check_subst;;

print_string 
"Usage:
* enter a lambda, theta and a variable to check if theta is free to substitute in lambda instead of variable
* enter 'exit' to exit
";;

while true do
	let l = read_line() in
	match l with
		| "exit" -> exit 0
		| _ ->
			let theta = read_line() in
			match theta with
			| "exit" -> exit 0
			| _ ->
				try
					let var = read_line() in
					match var with
					| "exit" -> exit 0
					| _ -> 
						if (free_subst (lambda_of_string theta) (lambda_of_string l) var) then
							print_string "true\n"
						else
							print_string "false\n"
				with
					| Parsing.Parse_error -> print_string "Parser failed, check your input\n"
					| Failure cause -> print_string "Lexer failed, check your input\n"
done;;