structure Typing =

struct
open AST
type typEnv = (id*typ) list
val lineNum = ref 0;

fun typEnvLookup (var: id, env: typEnv): typ =
	case List.find(fn(x, _) => x = var) env of
		SOME (x, v) => v
		| NONE => raise Fail ("Variable" ^ var ^ "is w/o a type")

fun typEnvAdd (var: id, t: typ, env: typEnv): typEnv =
	(var, t) :: env

fun getType(e: exp, env: typEnv): typ =
	(
	case e of
		NumExp _ => IntTyp
		| VarExp x => typEnvLookup(x, env)
		| Bool _ => BoolTyp
		| BinExp(b, e1, e2) => (let val t1 = getType(e1, env)
									val t2 = getType(e2, env)
								in if (t1 <> IntTyp orelse t2 <> IntTyp) then raise Fail ("Invalid type for bin exp in line: "^Int.toString(!lineNum)^", Expected: int type") else IntTyp
								end)
		| LetExp(ValDecl(x, e1), e2) => getType(e2, typEnvAdd(x, getType(e1, env), env))
		(*let e1Type = getType(e1, env)
										in (case e1Type of
												FnTyp(t1, t2) => getType(e2, *)
		| BoolExp(b, e1, e2) => (let val t1 = getType(e1, env)
									val t2 = getType(e2, env)
								in 
									(case b of
										EQUALS => if t1 <> t2 then raise Fail ("Type Mismatch in EQUALS exp in line: "^Int.toString(!lineNum)) else BoolTyp
										| LESSTHAN => if (t1 <> IntTyp orelse t2 <> IntTyp) then raise Fail ("Invalid type in LESSTHAN exp in line: "^Int.toString(!lineNum)^", Expected int type") else BoolTyp
										| GREATERTHAN => if (t1 <> IntTyp orelse t2 <> IntTyp) then raise Fail ("Invalid type in GREATERTHAN exp in line: "^Int.toString(!lineNum)^", Expected int type") else BoolTyp
										| _ => if (t1 <> BoolTyp orelse t2 <> BoolTyp) then raise Fail ("Invalid type for bool exp in line: "^Int.toString(!lineNum)^", Expected: bool type") else BoolTyp )
								(*if b <> EQUALS then if (t1 <> BoolTyp orelse t2 <> BoolTyp) then raise Fail "Invalid type for bool exp" else BoolTyp 
									else if t1 <> t2 then raise Fail "Invalid type for bool exp" else BoolTyp*)
								end)
		| NotExp e => (let val t = getType(e, env)
						in if t <> BoolTyp then raise Fail ("Invalid type for Not exp in line: "^Int.toString(!lineNum)^", Expected: bool type") else BoolTyp
						end)
		| NegateExp e => (let val t = getType(e, env)
						in if t <> IntTyp then raise Fail ("Invalid type for Negate exp in line: "^Int.toString(!lineNum)^", Expected: int type") else IntTyp
						end)
		| IfThenElse(e1, e2, e3) =>
									(let val t1 = getType(e1, env)
										val t2 = getType(e2, env)
										val t3 = getType(e3, env)
									in if t1 <> BoolTyp then raise Fail ("Condition in If statement is not Bool Type in line: "^Int.toString(!lineNum))
										else if t2 <> t3 then raise Fail ("Type Error: Branches in If exp has different types in line: "^Int.toString(!lineNum)) else t2  
									end)
		| AppExp(e1, e2) =>
							(case (getType(e1, env), getType(e2, env)) of
								(FnTyp(t1, t2), t3) => 
														if (t1 = t3) then t2 else raise Fail ("Application argument type mismatch in line: "^Int.toString(!lineNum))
								| (_,_) => raise Fail "Function was expected")
		| Fn(x, t1, t2, e) => (*FnTyp(t, getType(e, typEnvAdd(x, t1, env)))*)
								(let val eType = getType(e, typEnvAdd(x, t1, env))
									in if eType <> t2 then raise Fail ("Mismatch in declared type and actual Type in line: "^Int.toString(!lineNum)) else FnTyp(t1, t2)
									end)
		| Fun(fname, x1, t1, t2, e) => 
									(let val eType = getType(e, typEnvAdd(fname, t2, typEnvAdd(x1, t1, env)))
									in if eType <> t2 then raise Fail ("Mismatch in declared type and actual Type in line: "^Int.toString(!lineNum)) else FnTyp(t1, t2)
									end))

fun checkFile(file: exp list, env: typEnv): typ list =
	case file of
		f1 :: [] => (lineNum := !lineNum + 1; case getType(f1, env) of 
					(FnTyp(t1, t2)) => (t2::[])
					| _ => ( getType(f1, env)::[]))
		| f1 :: f2 => (lineNum := !lineNum + 1; case getType(f1, env) of
					(FnTyp(t1, t2)) => (
					case f1 of 
											Fun(fname, x1, type1, type2, e) => FnTyp(t1, t2)::checkFile(f2, typEnvAdd(fname, FnTyp(t1, t2) ,env))
											| _ => raise Fail ("Invalid in line: "^Int.toString(!lineNum)))
					| _ => getType(f1, env)::checkFile(f2, env))

end