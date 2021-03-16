structure ParserLrVals = ParseLrValsFun(structure Token = LrParser.Token)
structure ParserLex = ParsLexFun(structure Tokens = ParserLrVals.Tokens);
structure ParserParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = ParserLrVals.ParserData
     	       structure Lex = ParserLex)

fun invoke lexstream =
    	let fun print_error (s,pos:int,col:int) = ()
		    	(*TextIO.output(TextIO.stdOut,  s ^ ":" ^(Int.toString pos) ^ ":" ^(Int.toString col)^ "\n");*)
				
		in
		    ParserParser.parse(0,lexstream,print_error,())
		end

fun fileToLexer filename =
	let val inStream = TextIO.openIn(filename)
	in (TextIO.output(TextIO.stdOut, "["); ParserParser.makeLexer(fn n => TextIO.inputAll(inStream)))
	end	

fun parse (lexer) =
    let val dummyEOF = ParserLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = ParserParser.Stream.get lexer
    in
        if ParserParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parsefile = parse o fileToLexer


