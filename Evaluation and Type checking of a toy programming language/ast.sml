structure AST =
struct

type id = string

datatype binop = PLUS | MINUS | TIMES | EQUALS | LESSTHAN | GREATERTHAN | OR | XOR | AND | IMPLIES


datatype unop =  NEGATE | NOT 

datatype typ = INT | BOOL | ARROW of typ*typ | Type_error

datatype decl = ValDecl of id * exp | Fun of id*exp 



and exp = NumExp of int
    	| VarExp of id
	    | BinExp of binop * exp * exp
	    | LetExp of decl * exp
        | BoolExp of bool
        | UnExp of unop*exp
        | CondExp of exp*exp*exp
        | AppExp of exp*exp
        | Fn of id*typ*typ*exp


				       
datatype value = IntVal of int
	       | BoolVal of bool
           | Func of id*typ*typ*exp


datatype statement = Expr of exp | Decl of decl 

datatype program = Program of statement*program | Statement of  statement 


type environment = (id * value) list

type bound_environment = id list 

fun benvAdd (v , benv) = v :: benv

type typ_env = (id*typ) list 

fun envAdd (var, v, env) =
    (var,v)::env

fun envLookup (var, env) =
    case List.find(fn (x, _) => x = var) env of
				       SOME (x, v)   => v
				    |   NONE => raise Fail "Environment lookup error"				    


fun stringtoBinOp(e) = 
    case e of 
        PLUS => "PLUS"
        |MINUS => "MINUS"
        |TIMES => "TIMES"
        |EQUALS => "EQUALS"
        | LESSTHAN => "LESSTHAN " 
        | GREATERTHAN => "GREATERTHAN"
        | OR => "OR"
        | XOR => "XOR"
        | AND => "AND"
        | IMPLIES => "IMPLIES"


fun stringtoUnOp(NOT) = "NOT"
    | stringtoUnOp(NEGATE) = "NEGATE"


fun stringtoTyp(INT) = "INT"
    |stringtoTyp(BOOL) = "BOOL"
    |stringtoTyp(ARROW(e1,e2)) = "( "^stringtoTyp(e1)^"->"^stringtoTyp(e2)^" )"
    |stringtoTyp(Type_error) = "NOT WELL-TYPED"


fun stringtoDecl(ValDecl(i1,e1)) = "ValDecl( "^i1^", "^exptoString(e1)^") "
    |stringtoDecl(Fun(i1,e1)) = "Fun( "^i1^", "^exptoString(e1)^") "



and exptoString(NumExp(z)) = "NumExp "^Int.toString(z)
    |exptoString(VarExp(z)) = "VarExp "^z
    |exptoString(BoolExp(false)) ="BoolExp false"
    |exptoString(BoolExp(true)) = "BoolExp true"
    |exptoString(BinExp(binop,e1,e2)) = "BinExp( "^stringtoBinOp(binop)^", "^exptoString(e1)^", "^exptoString(e2)^") "
    |exptoString(LetExp(d,e)) = "LetExp( "^stringtoDecl d ^", "^ exptoString e ^") "
    |exptoString(UnExp(u,e)) = "UnExp( "^stringtoUnOp u ^", "^ exptoString e^") "
    |exptoString(CondExp(e1,e2,e3)) = "CondExp( "^exptoString e1^", "^exptoString e2^", "^exptoString e3^") "
    |exptoString (AppExp(e1,e2))  = "AppExp( "^exptoString e1 ^ ", "^exptoString e2^") "
    |exptoString (Fn(i1,t1,t2,e1)) = "Fn( "^i1^", "^stringtoTyp t1^", "^stringtoTyp t2^", "^exptoString e1^") "

 
fun statementtoString(Expr(z)) =  "Expr( "^exptoString z^") "
    |statementtoString(Decl(z)) = "Decl( "^stringtoDecl z^") " 

fun toString ( Program (s,p)) = "Program( "^statementtoString s^", "^toString p^") "
    |toString (Statement(s)) = " Statement( "^statementtoString s^") "


fun valtoString(IntVal(z)) = Int.toString z
    |valtoString(BoolVal(true)) = "true"
    |valtoString(BoolVal(false)) = "false"
    |valtoString(Func(i,t1,t2,e)) = "Func( "^i^", "^stringtoTyp t1^", "^stringtoTyp t2^", "^exptoString e^") "

fun listtoString (nil) = "\n"
    | listtoString(hd::nil) = valtoString(hd)^listtoString(nil)
    | listtoString(hd1::hd2::tl) = valtoString(hd1)^", "^listtoString(hd2::tl)

fun reverse(l) =
 let 
  fun f(nil,x) = x
    |f(hd::tl,x) = f(tl,hd::x)
  in
    f(l,nil)
  end


end


