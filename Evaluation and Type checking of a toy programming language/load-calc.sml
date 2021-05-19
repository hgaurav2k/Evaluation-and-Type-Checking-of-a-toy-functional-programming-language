structure CalcLrVals = CalcLrValsFun(structure Token = LrParser.Token)
structure CalcLex = CalcLexFun(structure Tokens = CalcLrVals.Tokens);
structure CalcParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = CalcLrVals.ParserData
     	       structure Lex = CalcLex)


fun invoke lexstream =
        let fun print_error (s,pos1:int,pos2:int) =
                TextIO.output(TextIO.stdOut, "Syntax Error:" ^ (Int.toString pos1) ^ ":" ^(Int.toString pos2)^":"^ "\n")
        in
            CalcParser.parse(0,lexstream,print_error,())
        end

fun stringToLexer str =
    let val done = ref false
    	val lexer=  CalcParser.makeLexer (fn _ => if (!done) then "" else (done:=true;str))
    in
	lexer
    end	
		
fun parse (lexer) =
    let val dummyEOF = CalcLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	val (nextToken, lexer) = CalcParser.Stream.get lexer
    in
        if CalcParser.sameToken(nextToken, dummyEOF) then result
 	else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end

val parseString = parse o stringToLexer

fun parser(filename) = let 
    open TextIO
    val file = inputAll(openIn filename) in parseString(file) end ; 

(*fun run (filename) = 
    let
        val s = toString((parser(filename)))
        val types = type_check ((parser filename))
        open List
    in
         if (exists (fn(a) =>(a = Type_error)) types) then stringtoTyp(Type_error) else 
            let 
                val values = evaluate tree
            in  
                if 

                    (exists (fn(a) =>(a = Failed)) values) 
                then s ^"\n"^"cannot be evaluated"
                
                else s ^"\n" ^ listtoString(values)
            end


    end
fun a3(filename) = print(run filename)
*)

