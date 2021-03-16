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
  val eof = fn () => (if !isError = 0 then (TextIO.output(TextIO.stdOut, "]\n");reset isError; resetline pos; reset colnum;Tokens.EOF(!pos, !colnum)) else (reset isError; resetline pos; reset colnum;raise UnknownTokens))
  val error = fn (e, l:int, _) => TextIO.output(TextIO.stdOut, "\n"^ e)
  fun refinc x =  (x := !x + 1; !x)
  fun refincby(x, y) = (x := !x + y, !x)

  
  
%%
%header (functor ParsLexFun(structure Tokens:Parse_TOKENS));

alpha=[A-Za-z];
ws = [\ \t];
%%
\n => (refinc pos; reset colnum; (if !isError = 0 then ((TextIO.output(TextIO.stdOut, ", "));lex()) else lex()));
{ws}+ => (if !isError = 0 then (refincby(colnum, size(yytext)); if !colnum > size(yytext) then ((TextIO.output(TextIO.stdOut, ", "));lex()) else lex()) else (refincby(colnum, size(yytext));lex()));
{alpha}+ => (refincby(colnum, size(yytext));
				if yytext = "TRUE" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "CONST"^" "^"\""^yytext^"\"");Tokens.CONST(true, !pos, !colnum)) else Tokens.CONST(true, !pos, !colnum))
				else if yytext = "FALSE" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "CONST"^" "^"\""^yytext^"\""); Tokens.CONST(false, !pos, !colnum)) else Tokens.CONST(false, !pos, !colnum))
				else if yytext = "NOT" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "NOT"^" "^"\""^yytext^"\"");Tokens.NOT(!pos, !colnum)) else Tokens.NOT(!pos, !colnum))
				else if yytext = "AND" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "AND"^" "^"\""^yytext^"\"");Tokens.AND(!pos, !colnum)) else Tokens.AND(!pos, !colnum))
				else if yytext = "OR" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "OR"^" "^"\""^yytext^"\"");Tokens.OR(!pos, !colnum)) else Tokens.OR(!pos, !colnum))
				else if yytext = "XOR" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "XOR"^" "^"\""^yytext^"\"");Tokens.XOR(!pos, !colnum)) else Tokens.XOR(!pos, !colnum))
				else if yytext = "EQUALS" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "EQUALS"^" "^"\""^yytext^"\"");Tokens.EQUALS(!pos, !colnum)) else Tokens.EQUALS(!pos, !colnum))
				else if yytext = "IMPLIES" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "IMPLIES"^" "^"\""^yytext^"\"");Tokens.IMPLIES(!pos, !colnum)) else Tokens.IMPLIES(!pos, !colnum))
				else if yytext = "IF" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "IF"^" "^"\""^yytext^"\"");Tokens.IF(!pos, !colnum)) else Tokens.IF(!pos, !colnum))
				else if yytext = "THEN" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "THEN"^" "^"\""^yytext^"\"");Tokens.THEN(!pos, !colnum)) else Tokens.THEN(!pos, !colnum))
				else if yytext = "ELSE" then (if !isError = 0 then (TextIO.output(TextIO.stdOut, "ELSE"^" "^"\""^yytext^"\"");Tokens.ELSE(!pos, !colnum)) else Tokens.ELSE(!pos, !colnum))
				else (TextIO.output(TextIO.stdOut, "ID"^" "^"\""^yytext^"\"");Tokens.ID(yytext, !pos, !colnum)));
";" => (if !isError = 0 then (refinc colnum; TextIO.output(TextIO.stdOut, ", TERM"^" "^"\""^yytext^"\""); Tokens.TERM(!pos, !colnum)) else Tokens.TERM(!pos, !colnum));
"(" => (if !isError = 0 then (refinc colnum; TextIO.output(TextIO.stdOut, "LPAREN"^" "^"\""^yytext^"\", "); Tokens.LPAREN(!pos, !colnum)) else Tokens.LPAREN(!pos, !colnum));
")" => (if !isError = 0 then (refinc colnum; TextIO.output(TextIO.stdOut, ", RPAREN"^" "^"\""^yytext^"\""); Tokens.RPAREN(!pos, !colnum)) else Tokens.RPAREN(!pos, !colnum));
. => (refinc colnum; error ("Unknown Token:"^Int.toString(!pos)^":"^Int.toString(!colnum)^":"^yytext, !linenum, !colnum); isError := (!isError) + 1; lex());
