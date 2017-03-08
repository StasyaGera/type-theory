open Ltype;;
open Parse_lambda;;
open Alpha_eq;;

print_string 
"Usage:
* enter two lambdas to check if they are alpha equivalent
* enter 'exit' to exit
";;

while true do
	let l1 = read_line() in
	match l1 with
		| "exit" -> exit 0
		| _ ->
			try
				let l2 = read_line() in
				match l2 with
				| "exit" -> exit 0
				| _ -> 
					if (alpha_eq (lambda_of_string l1) (lambda_of_string l2)) then
						print_string "true\n"
					else
						print_string "false\n"
			with
				| Parsing.Parse_error -> print_string "Parser failed, check your input\n"
				| Failure cause -> print_string "Lexer failed, check your input\n"
done;;