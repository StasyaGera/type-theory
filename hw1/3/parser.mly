%token <string> VAR
%token LAMBDA
%token DOT
%token WS
%token OPAREN CPAREN
%token EOF

%nonassoc DOT
%left WS

%start main
%type <Ltype.lambda> main
%%

main:
	| parse EOF { $1 }
;

parse:
	| VAR 					{ Var $1 }
	| OPAREN parse CPAREN 	{ $2 }
	| parse WS parse 		{ App ($1, $3) }
	| LAMBDA VAR DOT parse 	{ Abs ($2, $4) }
;