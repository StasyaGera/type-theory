open Ltype;;
open Parse_lambda;;

print_string 
"Usage:
* enter lambda to evaluate (string_of_lambda (lambda_of_string your_input))
* enter 'exit' to exit
";;

while true do
	let input = read_line() in
	match input with
		| "exit" -> exit 0
		| _ ->
			try
				print_string (
					"> " ^ 
					(string_of_lambda
						(lambda_of_string (input))) 
					^ "\n"
				)
			with
				| Parsing.Parse_error -> print_string "Parser failed, check your input\n"
				| Failure cause -> print_string "Lexer failed, check your input\n"
done;;