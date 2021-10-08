structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token
  exception UnknownTokens
  val linenum = ref 1
  val colnum = ref 0
  val pos = ref 1
  val isError = ref 0
  fun reset x = (x := 0, !x)
  fun resetline x = (x := 1, !x)
  val eof = fn () => (if !isError = 0 then (TextIO.output(TextIO.stdOut, "");reset isError; resetline pos; reset colnum;Tokens.EOF(!pos, !colnum)) else (reset isError; resetline pos; reset colnum;raise UnknownTokens))
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut, "\n"^ e)
  fun refinc x =  (x := !x + 1; !x)
  fun refincby(x, y) = (x := !x + y, !x)

%%
%header (functor ParsLexFun(structure Tokens:Parse_TOKENS));

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];
%%
\n => (refinc pos; reset colnum; lex());
{ws}+ => (refincby(colnum, size(yytext));lex());
{digit}+ => (Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !pos, !colnum));
{alpha}+{digit}* => (refincby(colnum, size(yytext));
			if yytext = "TRUE" then (Tokens.CONST(true, !pos, !colnum))
				else if yytext = "FALSE" then (Tokens.CONST(false, !pos, !colnum))
				else if yytext = "NOT" then (Tokens.NOT(!pos, !colnum))
				else if yytext = "AND" then (Tokens.AND(!pos, !colnum))
				else if yytext = "OR" then (Tokens.OR(!pos, !colnum))
				else if yytext = "XOR" then (Tokens.XOR(!pos, !colnum))
				else if yytext = "EQUALS" then (Tokens.EQUALS(!pos, !colnum))
				else if yytext = "IMPLIES" then (Tokens.IMPLIES(!pos, !colnum))
				else if yytext = "if" then (Tokens.IF(!pos, !colnum))
				else if yytext = "then" then (Tokens.THEN(!pos, !colnum))
				else if yytext = "else" then (Tokens.ELSE(!pos, !colnum))
				else if yytext = "PLUS" then (Tokens.PLUS(!pos, !colnum))
				else if yytext = "MINUS" then (Tokens.MINUS(!pos, !colnum))
				else if yytext = "TIMES" then (Tokens.TIMES(!pos, !colnum))
				else if yytext = "NEGATE" then (Tokens.NEGATE(!pos, !colnum))
				else if yytext = "LESSTHAN" then (Tokens.LESSTHAN(!pos, !colnum))
				else if yytext = "GREATERTHAN" then (Tokens.GREATERTHAN(!pos, !colnum))
				else if yytext = "let" then (Tokens.LET(!pos, !colnum))
				else if yytext = "in" then (Tokens.IN(!pos, !colnum))
				else if yytext = "end" then (Tokens.END(!pos, !colnum))
				else if yytext = "fi" then (Tokens.FI(!pos, !colnum))
				else if yytext = "fun" then (Tokens.FUN(!pos, !colnum))
				else if yytext = "fn" then (Tokens.FN(!pos, !colnum))
				else if yytext = "int" then (Tokens.INT(!pos, !colnum))
				else if yytext = "bool" then (Tokens.BOOL(!pos, !colnum))
				else (Tokens.ID(yytext, !pos, !colnum)));	
";" => (Tokens.TERM(!pos, !colnum));
"(" => (Tokens.LPAREN(!pos, !colnum));
"=>" => (Tokens.IMPLY(!pos, !colnum));
"->" => (Tokens.ARROW(!pos, !colnum));
")" => (Tokens.RPAREN(!pos, !colnum));
"=" => (Tokens.EQ(!pos, !colnum));
":" => (Tokens.COLON(!pos, !colnum));
. => (refinc colnum; error ("Unknown Token:"^Int.toString(!pos)^":"^Int.toString(!colnum)^":"^yytext, !linenum, !colnum); isError := (!isError) + 1; lex());
