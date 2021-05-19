val args = CommandLine.arguments()
open AST
open EVALUATOR
open TYPE_CHECKER
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


fun a3(filename) = 
    let 
        val ast = parser(filename)
    in 
        (print(toString(ast)) ; print("\n");
           let
                val types = type_check(ast)
                val eval  = evaluate(ast) 
            in 
                (   
                    if hd types = Type_error then print("")
                    else let val s = (listtoString(eval)) in print(s) end
                )
            end
        )
    end;

a3(hd(args));

