
(* TODO :
 1 - Check no of fields in each line
 2 - Enclose the field in a double quote if it has delim2
 3 - Difficult, but try : If field has a double quote followed by delimiter or newline
 4 - in_quotes for 1st character of a line is taken as false, change it!
 5 - Write all TODOs here
 *)


(*
@ param infilename: name of input file
@ param delim1: Delimiter of input file
@ param outfilename: name of output file
@ param delim2: Delimiter of output file
*)

fun convertDelimiters(infilename: string, delim1: char, outfilename: string, delim2: char) =
    let
	val inStream = TextIO.openIn infilename
	val outStream = TextIO.openOut outfilename
				       
	(* This function splits the given line by given delimiter "delim" *)
	fun splitter delim d = String.tokens (fn d => d = delim) d

	fun isDelim1(c: char) =
	    if c = delim1 then 1 else ~1
					   
	fun isNewline(c: char) =
	    if c = #"\n" then 1 else ~1
					  
	fun isQuote(c: char) =
	    if c = #"\"" then 1 else ~1 
					  
	(* To Write *)
	exception EOLNotFound
	fun readChar(ch: char option, in_quotes: bool) =
	    case ch of
		NONE => (TextIO.closeIn inStream; TextIO.closeOut outStream)
	      | SOME(c) => let val next_ch = TextIO.input1 inStream
			       val x = [isDelim1(c), isQuote(c), isNewline(c)]
			       val y = (case next_ch of
					   NONE => [0, 0]
					 | SOME(d) => [isDelim1(d), isNewline(d)])
			   in
			       (case x of
				   [1,_,_] => if in_quotes = true
					      then
						  (TextIO.output1(outStream, c); readChar(next_ch, in_quotes))
					      else
						  (case next_ch of
						      NONE => raise EOLNotFound
						    | SOME(cha) => case cha of
								       #"\n" => raise EOLNotFound
								     | #"\"" => (TextIO.output1(outStream, delim2); readChar(next_ch, true))
								     | _ => (TextIO.output1(outStream, delim2); readChar(next_ch, false)))
										
			        | [_,1,_] => (case y of						      
						  [0,0] => raise EOLNotFound
						| [1,_] => (TextIO.output1(outStream, c); readChar(next_ch, false))
						| [_,1] => (TextIO.output1(outStream, c); readChar(next_ch, false)) 
						| [_,_] => (TextIO.output1(outStream, c); readChar(next_ch, in_quotes)))
				| [_,_,1] => if in_quotes = true
						 then
							(TextIO.output1(outStream, c); readChar(next_ch, in_quotes))
						 else
							(TextIO.output1(outStream, c); readChar(next_ch, in_quotes)) (* To Check No. of Fields *)
				| [_,_,_] => (TextIO.output1(outStream, c); readChar(next_ch, in_quotes)))   
			   end
    in
	readChar(TextIO.input1 inStream, false)
    end
	
			   
