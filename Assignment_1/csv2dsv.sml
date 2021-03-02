
(* TODO :
 1 - Check no of fields in each line - DONE
 2 - Enclose the field in a double quote if it has delim2 - DONE
 3 - If field has a double quote followed by delimiter or newline - DONE
 4 - Prinitng Exception
 5 - Write all TODOs here
 *)


(*
@ param infilename: name of input file
@ param delim1: Delimiter of input file
@ param outfilename: name of output file
@ param delim2: Delimiter of output file
*)

(*
 Algorithm:

 We have to change the delim1 to delim2 which are outside fields. If delim1 is inside field, then we should not change the fields. 
 If no of quotes before delim1 is even then that delim1 is outside the field. If no of quotes before delim1 is odd then that delim 1
 is inside a field.
 
 Step - 1: Read each character from the input file

 Step - 2: If the character is delim1 then check no of quotes before the delim1. If it is even then replace delim1 by delim2 in output
           file. If it is odd, print delim1 in output file.

 Step - 3: If character is double quotes, then update the no_quotes variable.

 Step - 4: If character is newline and if it is outside a field, check if no of fields matches with expected fields.
           If there is a mismatch, raise UnevenFields exception.

 Step - 5: If it is any other character, print the same character in output file.

*)

fun convertDelimiters(infilename: string, delim1: char, outfilename: string, delim2: char) =
    let
	val inStream = TextIO.openIn infilename
				     
	val outStream = TextIO.openOut outfilename

	(*This function checks if the gven character is delim1 or not*)

	fun isDelim1(c: char) =
	    
	    if c = delim1 then 1 else ~1

	(*This function checks if the given character is newline or not*)
					   
	fun isNewline(c: char) =
	    
	    if c = #"\n" then 1 else ~1

	(*This function checks if the given character is double quote or not*)
					 
	fun isQuote(c: char) =
	    
	    if c = #"\"" then 1 else ~1 
					  
	(* If the document donot end with \n then program rises EOLNotFound exception *)
					 
	exception EOLNotFound

	(*If the input file is empty, then program rises emptyInputFile exception*)
		      
	exception emptyInputFile

	(*If no of fields in each line of input file are not same, then program rises UnevenFields exception*)
		      
	exception UnevenFields

	(*
	
	@param ch: Input character taken at each time
	@param no_quotes: Is no of double quotes that occured before the present character
	@param no_fields: Is no_fields in the given line
	@param exp_fields: Is expected no of fields that each line should have
	@param line_no: Denotes the present line
	@param prev_ch: Is the previous character to the present character
	@param is_first: checks whether the present character is the first character of a line or not

	*)
		      
	fun readChar(ch: char option, no_quotes: int, no_fields: int, exp_fields: int, line_no: int, prev_ch: char list, is_first: bool) =
	    
	    case ch of

		(* If we find the end of file then if prev_ch is null, then the file is empty
		   If the prev_ch is \n, close the input and output files
		   If prev_ch is not \n, the raise EOLNotFound exception *)
		
		NONE => if null prev_ch = true
					      
			then
			    raise emptyInputFile
				  
			else
			    
			    if hd prev_ch = #"\n"
						
			    then
				(TextIO.closeIn inStream; TextIO.closeOut outStream)
				    
			    else
				raise EOLNotFound
				      
	      | SOME(c) => let val next_ch = TextIO.input1 inStream
							   
			       val x = [isDelim1(c), isQuote(c), isNewline(c)]
					   
			       val y = (case next_ch of
					    
					    NONE => [0, 0]
							
					  | SOME(d) => if is_first = true
									        
						      then [~1, 0]
							       
						      else [isQuote(hd prev_ch), isQuote(d)])
					   
			   in
			       (case x of

				    (*If the present character is delim1 and no of quotes before is even then replace it with delim2*)
				    (*If no of quotes is odd, then print delim1 in output file*)
				    
				    [1,_,_] => if no_quotes mod 2 = 0
									
					       then

						   (* This adds double quotes to the field if it not in double quotes *)
						   case y of
						      
						       [0, 0] => raise EOLNotFound
								       
						     | [~1, 0] => (TextIO.output1(outStream, #"\"");
								   TextIO.output1(outStream, #"\"");
								   TextIO.output1(outStream, delim2);
								   readChar(next_ch, no_quotes, no_fields + 1, exp_fields, line_no, [delim2], false))
								      
						    | [~1, ~1] => (TextIO.output1(outStream, #"\"");
								   TextIO.output1(outStream, delim2);
								   TextIO.output1(outStream, #"\"");
								   readChar(next_ch, no_quotes, no_fields + 1, exp_fields, line_no, [delim2], false))
								      
						    | [~1, 1]  => (TextIO.output1(outStream, #"\"");
								   TextIO.output1(outStream, delim2);
								   readChar(next_ch, no_quotes, no_fields + 1, exp_fields, line_no, [delim2], false))
								      
						    | [1, ~1]  => (TextIO.output1(outStream, delim2);
								   TextIO.output1(outStream, #"\"");
								   readChar(next_ch, no_quotes, no_fields + 1, exp_fields, line_no, [delim2], false))
								      
						    | [1, 1]   => (TextIO.output1(outStream, delim2);
								   readChar(next_ch, no_quotes, no_fields + 1, exp_fields, line_no, [delim2], false))
								      
					      else
						  (TextIO.output1(outStream, c);
						   readChar(next_ch, no_quotes, no_fields, exp_fields, line_no, [c], false))

				  (* If present character is double quotes, increment the variable no_quotes*)
						      
			         | [_,1,_] => (TextIO.output1(outStream, c);
					       readChar(next_ch, no_quotes + 1, no_fields, exp_fields, line_no, [c], false))

				 (*If present character is newline, then if no_quotes is odd, then the character is inside the field*)
				 (*If no_quotes is even, then the character is outside the field
				   In this case check whether no of fields and expected fields are same
				   If they are same, increment line_no and go to next line*)
						  
				 | [_,_,1] => if no_quotes mod 2 = 0
								       
					     then
						
						 if line_no = 1 then

						     if is_first = true
								       
						     then
							 (TextIO.output1(outStream, #"\"");
							  TextIO.output1(outStream, #"\"");
							  TextIO.output1(outStream, c);
							  readChar(next_ch, 0, 1, no_fields, line_no + 1, [c], true))
							     
						     else
							 
							 if hd prev_ch = #"\""
									     
							 then
							     (TextIO.output1(outStream, c); 
							      readChar(next_ch, 0, 1, no_fields, line_no + 1, [c], true))
								 
							 else
							     (TextIO.output1(outStream, #"\"");
							      TextIO.output1(outStream, c);
							      readChar(next_ch, 0, 1, no_fields, line_no + 1, [c], true))
	 							 
						 else
						     
						     if no_fields = exp_fields
									
						     then
							 
							 if is_first = true
									   
							 then
							     (TextIO.output1(outStream, #"\"");
							      TextIO.output1(outStream, #"\"");
							      TextIO.output1(outStream, c);
							      readChar(next_ch, 0, 1, exp_fields, line_no + 1, [c], true))
								 
							 else
							     
							     if hd prev_ch = #"\""
										 
							     then
								 (TextIO.output1(outStream, c); 
								  readChar(next_ch, 0, 1, exp_fields, line_no + 1, [c], true))
								     
							     else
								 (TextIO.output1(outStream, #"\"");
								  TextIO.output1(outStream, c);
								  readChar(next_ch, 0, 1, exp_fields, line_no + 1, [c], true))
							  
						     else
							 (print("Expected: " ^ Int.toString(exp_fields) ^ " fields, Present: " ^ Int.toString(no_fields) ^ " fields on Line " ^ Int.toString(line_no));
							 raise UnevenFields)
							       
					     else	 
						 (TextIO.output1(outStream, c);
						  readChar(next_ch, no_quotes, no_fields,exp_fields, line_no, [c], false))

				 (*If the present character is anything else, add it to the output file*)
				 (*Take care if the charcter is the first character of the line*)
						     
				 | [_,_,_] => if is_first = true
								
					      then
						  
						  if c = #"\""
							     
						 then (TextIO.output1(outStream, c);
						       readChar(next_ch, no_quotes, no_fields, exp_fields, line_no, [c], false))
							  
						 else
						     (TextIO.output1(outStream, #"\"");
						      TextIO.output1(outStream, c);
						      readChar(next_ch, no_quotes, no_fields, exp_fields, line_no, [c], false))
							 
					     else
						 (TextIO.output1(outStream, c);
						  readChar(next_ch, no_quotes, no_fields, exp_fields, line_no, [c], false)))
				   
			   end
			       
    in
	
	readChar(TextIO.input1 inStream, 0, 1, 0, 1, [], true)
		
    end

(*Converts a given csv file to tsv file using convertDelimiters function*)

fun csv2tsv(infilename: string, outfilename: string) =
    convertDelimiters(infilename, #",", outfilename, #"\t")

(*Converts a given tsv file to csv file using convertDelimiters function*)
		     
fun tsv2csv(infilename: string, outfilename: string) =
    convertDelimiters(infilename, #"\t", outfilename, #",")
	
			   
