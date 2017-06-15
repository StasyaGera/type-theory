{
	open Parser
}

rule token = parse
	| "\\\\" 				{ LAMBDA }
	| ['a'-'z']+ as name	{ VAR name }
	| '(' 					{ OPAREN }
	| ')' 					{ CPAREN }
	| '.' 					{ DOT }
	| [' ']+				{ WS }
	| eof					{ EOF }