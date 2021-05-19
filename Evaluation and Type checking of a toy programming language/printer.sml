(*val args = CommandLine.arguments()*)
fun a3(filename) = 
    let 
        val ast = parser(filename)
        val types = type_check(ast)
        val eval  = evaluate(ast) 
    in 
        (   print(toString(ast)) ; print("\n") ; 
            if hd types = Type_error then print("\n")
            else let val s = (listtoString(eval)) in print(s) end
        )
    end ;  

(*a3(hd(args));*)

