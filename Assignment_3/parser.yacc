(* User  declarations *)

%%
(* required declarations *)
%name Parse

%term
	CONST of bool| EOF | TERM | ID of string | NOT | AND | OR | XOR | INT | BOOL | COLON |
	EQUALS | IMPLIES | IF | THEN | ELSE | LPAREN | RPAREN | NUM of int | FI | FUN | FN |
	PLUS | MINUS | TIMES | NEGATE | LESSTHAN | GREATERTHAN | LET | IN | END | EQ | IMPLY | ARROW

%nonterm formula of AST.exp | statement of AST.exp list | program of AST.exp list | decl of AST.decl | typ of AST.typ

%pos int


(*optional declarations *)
%eop EOF
%noshift EOF

(* %header  *)

%right ARROW
%nonassoc IMPLY

%left LESSTHAN GREATERTHAN
%left PLUS MINUS
%left TIMES
%right NEGATE
%right IF THEN ELSE
%right IMPLIES
%left AND OR XOR EQUALS
%right NOT
%left LPAREN RPAREN
%start program

%verbose

%%
program: statement (statement)
statement: formula (formula :: [])
		| formula TERM statement (formula :: statement)
		
decl: ID EQ formula(AST.ValDecl(ID, formula))

typ: typ ARROW typ (AST.FnTyp(typ1, typ2))
		| INT (AST.IntTyp)
		| BOOL (AST.BoolTyp)
		| LPAREN typ RPAREN (typ)
formula: 
	CONST (AST.Bool(CONST))
	| LPAREN formula RPAREN (formula1)
	| NOT formula (AST.NotExp(formula1))
	| formula AND formula (AST.BoolExp(AST.AND, formula1, formula2))
	| formula OR formula (AST.BoolExp(AST.OR, formula1, formula2))
	| formula XOR formula (AST.BoolExp(AST.XOR, formula1, formula2))
	| formula EQUALS formula (AST.BoolExp(AST.EQUALS, formula1, formula2))
	| formula IMPLIES formula (AST.BoolExp(AST.IMPLIES, formula1, formula2))
	| IF formula THEN formula ELSE formula FI (AST.IfThenElse(formula1, formula2, formula3))
	| ID (AST.VarExp(ID))
	| formula PLUS formula (AST.BinExp(AST.PLUS, formula1, formula2))
	| formula MINUS formula (AST.BinExp(AST.MINUS, formula1, formula2))
	| formula TIMES formula (AST.BinExp(AST.TIMES, formula1, formula2))
	| NEGATE formula (AST.NegateExp(formula1))
	| formula LESSTHAN formula (AST.BoolExp(AST.LESSTHAN, formula1, formula2))
	| formula GREATERTHAN formula (AST.BoolExp(AST.GREATERTHAN, formula1, formula2))
	| LET decl IN formula END(AST.LetExp(decl, formula))
	| NUM(AST.NumExp(NUM))
	| LPAREN formula formula RPAREN (AST.AppExp(formula1, formula2))
	| FN LPAREN ID COLON typ RPAREN COLON typ IMPLY formula (AST.Fn(ID, typ1, typ2, formula))
	| FUN ID LPAREN ID COLON typ RPAREN COLON typ IMPLY formula (AST.Fun(ID1, ID2, typ1, typ2, formula))

