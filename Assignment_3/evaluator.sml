structure EVALUATOR  =
struct
open AST

val brokenTypes = Fail "Error in evaluation!"
val lineNum = ref 0;
fun envAdd (var:id, v:value, env:environment) =
    (var,v)::env

fun envLookup (var:id, env:environment) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail ("Environment lookup error in line: "^Int.toString(!lineNum))	
(*val i_list: id_list = [];
fun print_list(id_list: id_list) =
	case id_list of
		[] => TextIO.output(TextIO.stdOut, "2\n")
		| f1 :: f2 => (TextIO.output(TextIO.stdOut, f1^"1 "); print_list(f2))	*)
fun evalExp(e:exp, env:environment):value =
    case e of
		NumExp i => IntVal i
		| VarExp x => envLookup (x, env) 	
		| Bool b => BoolVal b
		| BinExp (b, e1, e2)  => evalBinExp(b, e1, e2, env)
		| LetExp(ValDecl(x, e1), e2)  =>
		let
			val v1 = evalExp (e1, env)
		in
				evalExp(e2, envAdd (x, v1, env))
        end
		| BoolExp (b, e1, e2) => evalBinExp(b, e1, e2, env)
		| NotExp b => evalNot(b, env)
		| NegateExp n => evalNegate(n, env)
		| IfThenElse (b, f1, f2) => evalIfThenElse(b, f1, f2, env)
		| AppExp (f1, f2) => evalApp(f1, f2, env)
		| Fun(id1, id2, typ1, typ2, e1) =>  FN_Exp (id2, e1, ref [])
		| Fn(id1, typ1, typ2, e1) => FN_Exp(id1, e1, ref env)
											
											
and
evalBinExp(b:binop, e1:exp, e2:exp, env:environment):value =
case (b, evalExp(e1, env), evalExp(e2, env))  of
    (PLUS, IntVal i1, IntVal i2) => IntVal (i1+i2)
  |   (MINUS, IntVal i1, IntVal i2) => IntVal (i1-i2)
  |   (TIMES, IntVal i1, IntVal i2) => IntVal (i1*i2)
  |   (LESSTHAN, IntVal i1, IntVal i2) => BoolVal (i1 < i2)
  |   (GREATERTHAN, IntVal i1, IntVal i2) => BoolVal(i1 > i2)
  |   (AND, BoolVal b1, BoolVal b2) => BoolVal (b1 andalso b2)
  |   (OR, BoolVal b1, BoolVal b2) => BoolVal (b1 orelse b2)
  |   (XOR, BoolVal b1, BoolVal b2) => BoolVal (b1 orelse b2 andalso not(b1 andalso b2))
  |   (EQUALS, BoolVal b1, BoolVal b2) => BoolVal (b1 = b2)
  |   (EQUALS, IntVal b1, IntVal b2) => BoolVal (b1 = b2)
  |   (IMPLIES, BoolVal b1, BoolVal b2) => BoolVal ((not b1) orelse b2)
  |   _  => raise Fail ("Error in evaluation! in line: "^Int.toString(!lineNum)) 	
and
evalIfThenElse(b: exp, f1: exp, f2: exp, env: environment): value =
	case (evalExp(b, env), evalExp(f1, env), evalExp(f2, env)) of
		(BoolVal b1, IntVal i1, IntVal i2) => if b1 then IntVal i1 else IntVal i2
		| (BoolVal b1, BoolVal b2, BoolVal b3) => if b1 then BoolVal b2 else BoolVal b3
		| _ => raise Fail ("Error in evaluation! in line: "^Int.toString(!lineNum))
and
evalNegate(n: exp, env: environment): value =
	case evalExp(n, env) of
		IntVal i => IntVal (0 - i)
		| _ => raise Fail ("Error in evaluation! in line: "^Int.toString(!lineNum))
and
evalNot(b: exp, env: environment): value =
	case evalExp(b, env) of
		BoolVal b1 => BoolVal (not b1)
		| _ => raise Fail ("Error in evaluation! in line: "^Int.toString(!lineNum))
and
evalApp(f1: exp, f2: exp, env: environment): value =
	case (evalExp(f1, env), evalExp(f2, env)) of
		(FN_Exp(x, e, enclosingEnv), IntVal i) => evalExp(e, envAdd(x, IntVal i, !enclosingEnv))
		| (FN_Exp(x, e, enclosingEnv), BoolVal i) => evalExp(e, envAdd(x, BoolVal i, !enclosingEnv))
		| (FN_Exp(x1, e1, enclosingEnv1), FN_Exp(x2, e2, enclosingEnv2)) => evalExp(e1, envAdd(x1, FN_Exp(x2, e2, enclosingEnv2), !enclosingEnv1))
		| _ => raise Fail ("Error in evaluation! in line: "^Int.toString(!lineNum))

fun evalFile(file: exp list, env: environment): value list =
	case file of
		f1 :: [] => (lineNum := !lineNum + 1;case evalExp(f1, env) of 
					(FN_Exp(id1, e1, enclosingEnv)) => []
					| _ => evalExp(f1, env)::[])
		| f1 :: f2 => (lineNum := !lineNum + 1;case evalExp(f1, env) of
					(FN_Exp(id1, e1, enclosingEnv)) => (case f1 of
												Fun(fname, x1, type1, type2, e) => evalFile(f2, envAdd(fname, FN_Exp(id1, e1, enclosingEnv), env))
												| _ => raise Fail ("Invalid in line: "^Int.toString(!lineNum)))
					| _ => evalExp(f1, env)::evalFile(f2, env))

end
