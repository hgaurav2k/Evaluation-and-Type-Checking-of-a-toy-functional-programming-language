structure EVALUATOR  =
struct
open AST
open List
exception brokenTypes 

fun callByValue(exp,env,bound_env) = 
  case exp of 
    VarExp(z) => (if (exists (fn(a) =>(a = z)) bound_env)
                 then VarExp(z) 
                 else case envLookup(z,env) of 
                  IntVal(z) => NumExp(z)
                  |BoolVal(z) => BoolExp(z)
                  |Func(z) => Fn(z)
                  )
    | BoolExp(z) => BoolExp(z)
    | NumExp(z) => NumExp(z)
    | Fn(id1,t1,t2,e) => Fn(id1,t1,t2,callByValue(e,env,benvAdd(id1,bound_env)))
    | AppExp(e1,e2) => AppExp(callByValue(e1,env,bound_env),callByValue(e2,env,bound_env))
    | LetExp(ValDecl(x,e1),e2) => LetExp(ValDecl(x,callByValue(e1,env,bound_env)),callByValue(e2,env,benvAdd(x,bound_env)))
    | LetExp(Fun(x,e1),e2) => LetExp(Fun(x,callByValue(e1,env,bound_env)),callByValue(e2,env,benvAdd(x,bound_env)))
    | BinExp(b1,e1,e2) => BinExp(b1,callByValue(e1,env,bound_env) , callByValue(e2,env,bound_env))
    | UnExp(u1,e1) => UnExp(u1 , callByValue(e1,env,bound_env))
    | CondExp(e1,e2,e3) => CondExp(callByValue(e1,env,bound_env),callByValue(e2,env,bound_env),callByValue(e3,env,bound_env))
    
fun evalExp(exp , env , bound_env) : value = 
  case exp of 
    NumExp(z) => IntVal(z)
    | Fn(id1,t1,t2,e) => Func( id1 , t1 , t2 , callByValue(e,env,benvAdd(id1,bound_env)) ) 
    | VarExp(z) => envLookup(z,env)
    | BoolExp(z) => BoolVal(z)
    | AppExp ( e1 , e2 ) => 
      let 
        val f = evalExp(e1,env , bound_env)
      in
        case f of Func(id1,t1,t2,e3) => 
        evalExp(e3, envAdd(id1,evalExp(e2,env,bound_env),env) , bound_env)
        | _ => raise brokenTypes
      end
    | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = evalExp (e1, env , bound_env)
	  in
	    evalExp(e2, envAdd (x, v1, env) , bound_env)
    end
    | LetExp(Fun(x,e1) , e2) =>
    let 
      val v1 = evalExp(e1 , env , bound_env)
    in
      evalExp(e2,envAdd(x,v1,env) , bound_env)
    end		   
    | _ => evalOp(exp , env , bound_env)


and 
  
evalOp(exp,env , bound_env) = 
let 
    fun evalUnExp(up , exp , env , bound_env ) = 
      case up of 
        NEGATE => 
          (case evalExp(exp,env , bound_env) of 
            IntVal(z) => IntVal((~1)*z)
            | _ => raise brokenTypes
          ) 

      |NOT => 
          (case evalExp(exp,env , bound_env) of 
            BoolVal(z) => BoolVal(not z)
            | _ => raise brokenTypes
          )

    fun evalBinExp(exp1, binop , exp2 , env , bound_env ) = 
      let
        fun getInt(v) =
          case v of 
            IntVal (z) => z 
            | _ => raise brokenTypes
          

        fun getBool(v) = 
          case v of 
            BoolVal(z) => z  
            | _ => raise brokenTypes
          

        val e1 = evalExp(exp1 , env , bound_env) 
        val e2 = evalExp(exp2 , env , bound_env )
      in 
        case binop of 
          PLUS => IntVal(getInt(e1) + getInt(e2))
          |MINUS => IntVal(getInt(e1) - getInt(e2))
          |EQUALS => (
              case e1 of 
              IntVal(x)=> BoolVal(e1=e2)
              |BoolVal(x) => BoolVal(e1 = e2)
              )
          |TIMES => IntVal(getInt(e1)*getInt(e2))
          |LESSTHAN => BoolVal(getInt(e1) < getInt(e2))
          |GREATERTHAN => BoolVal(getInt(e1) > getInt(e2))
          |AND => BoolVal(getBool(e1) andalso getBool(e2))
          |OR => BoolVal(getBool(e1) orelse getBool(e2))
          |IMPLIES => BoolVal(not(getBool(e1)) orelse (getBool(e1) andalso getBool(e2)))
          |XOR => BoolVal((getBool(e1) andalso not(getBool(e2))) orelse (getBool(e2) andalso not(getBool(e1))))
      end
      

    fun evalCondExp(exp1, exp2 , exp3 , env , bound_env ) = 
      case evalExp(exp1 , env , bound_env) of
        BoolVal(true) => evalExp(exp2 , env , bound_env )
        |BoolVal(false) => evalExp(exp3 , env , bound_env )
        | _ => raise brokenTypes
in
  case exp of 
    BinExp(binop , exp1 , exp2) => evalBinExp(exp1,binop,exp2,env , bound_env)
    |UnExp(up1, exp1) =>  evalUnExp(up1 , exp1 , env , bound_env)
    |CondExp(exp1, exp2 , exp3) => evalCondExp(exp1 , exp2 , exp3 , env , bound_env)
end




fun evaluate(program) = 
  let 
    fun f(program , env , bound_env, l ) = 
    case program of 
      Program(s1,p1) => 
        (case s1 of 
          
               (Expr(exp)) =>  f(p1 ,env , bound_env , evalExp(exp,env,bound_env) :: l )
              |(Decl(ValDecl(id,exp))) => f(p1 , envAdd(id,evalExp(exp,env,bound_env),env) , bound_env, l )
              |(Decl(Fun(id,exp))) => f(p1 , envAdd(id,evalExp(exp,env,benvAdd(id,bound_env)),env) , bound_env, l )
        )
      |Statement(Expr(exp)) => evalExp(exp,env , bound_env) :: l 
      |_ => l
      
  in 
    reverse(f(program , nil , nil, nil ))

    handle brokenTypes => []
  end


end


