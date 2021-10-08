structure AST =
struct

type id = string

datatype binop = AND | OR | XOR | EQUALS | IMPLIES | PLUS | MINUS | TIMES | LESSTHAN | GREATERTHAN 
(*datatype types = INT | BOOL*)
datatype typ = IntTyp | BoolTyp
		| FnTyp of typ*typ
datatype decl = ValDecl of id * exp



and exp = NumExp of int
    	| VarExp of id
		| Bool of bool
		| BinExp of binop * exp * exp
		| LetExp of decl * exp
        | BoolExp of binop * exp * exp
		| NotExp of exp
		| NegateExp of exp
		| IfThenElse of exp*exp*exp
		| AppExp of exp*exp
		| Fn of id*typ*typ*exp
		| Fun of id*id*typ*typ*exp
		

			       


type fun_exp = (id*exp)
type fun_list = (id*fun_exp) list	
type fn_list = (id*exp) list
datatype value = IntVal of int
			| BoolVal of bool | FN_Exp of id*exp*((id*value) list ref) (*| FUN_Exp of id*(id*exp)| Unit of unit | FUN_List of fun_list | FN_Exp of fun_exp*)
type environment = (id * value) list
								
end


