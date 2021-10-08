open AST
open EVALUATOR
open Typing
structure ParserLrVals = ParseLrValsFun(structure Token = LrParser.Token)
structure ParserLex = ParsLexFun(structure Tokens = ParserLrVals.Tokens);
structure ParserParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = ParserLrVals.ParserData
     	       structure Lex = ParserLex)

fun invoke lexstream =
    	let fun print_error (s,pos:int,_) =
		    	TextIO.output(TextIO.stdOut, "Error, line " ^ (Int.toString pos) ^ "," ^ s ^ "\n")
				
		in
		    ParserParser.parse(0,lexstream,print_error,())
		end

fun fileToLexer filename =
	let val inStream = TextIO.openIn(filename)
	in (ParserParser.makeLexer(fn n => TextIO.inputAll(inStream)))
	end	

fun parse (lexer) =
    let val dummyEOF = ParserLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = ParserParser.Stream.get lexer
    in
        if ParserParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

fun checkAST(ast: exp list) =
	checkFile(ast, [])

fun evalAST(ast: exp list) =
	evalFile(ast, [])

fun print1s(s) = ((print(s^"\n");true))
fun printsl([]) = true
        | printsl(h::t) = print1s(h) andalso printsl(t)
		
fun parsefiles (file)(*: ParserParser.result*string*typ list*string*value list*) =
	let val v1 = (parse o fileToLexer)(file);
		val v2 = (checkAST o parse o fileToLexer)(file);
		val v3 = (evalAST o parse o fileToLexer)(file);
	in
		("AST:", v1, "TYPES:", v2, "EVALUATION:", v3)
		
	end
(*val parsefile = (parse o fileToLexer; checkAST o parse o fileToLexer; evalAST o parse o fileToLexer)*)


