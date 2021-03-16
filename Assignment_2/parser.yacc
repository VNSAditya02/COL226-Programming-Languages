(* User  declarations *)

%%
(* required declarations *)
%name Parse

%term
	CONST of bool| EOF | TERM | ID of string | NOT | AND | OR | XOR | 
	EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN

%nonterm formula of string | statement of string | program of string | start of unit

%pos int


(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%left LPAREN RPAREN
%start start

%verbose

%%
start: program(TextIO.output(TextIO.stdOut, "[");TextIO.output(TextIO.stdOut, program);TextIO.output(TextIO.stdOut, "]\n"))
program: statement (statement^"program -> statements")
		| statement program (statement^program)
statement: formula TERM (formula^"TERM ;, "^"statement -> formula, ")
formula: 
	CONST ("CONST "^Bool.toString(CONST)^", "^"formula -> CONST ,")
	| LPAREN formula RPAREN ("LPAREN "^  "LPAREN, "^formula^"RPAREN "^"RPAREN, "^"formula -> (formula) ,")
	| NOT formula ("NOT "^"NOT, "^formula1^"formula -> NOT formula ,")
	| formula AND formula (formula1^"AND "^"AND, "^formula2^"formula -> formula AND formula ,")
	| formula OR formula (formula1^"OR "^"OR, "^formula2^"formula -> formula OR formula ,")
	| formula XOR formula (formula1^"XOR "^"XOR, "^formula2^"formula -> formula XOR formula ,")
	| formula EQUALS formula (formula1^"EQUALS "^"EQUALS, "^formula2^"formula -> formula EQUALS formula ,")
	| formula IMPLIES formula (formula1^"IMPLIES "^"IMPLIES, "^formula2^"formula -> formula IMPLIES formula ,")
	| IF formula THEN formula ELSE formula ("IF "^"IF, "^formula1^"THEN "^"THEN, "^formula2^"ELSE "^"ELSE, "^formula3^"formula -> IF formula THEN formula ELSE formula ,")
	| ID ("ID "^ID^", "^"formula -> ID ,")
