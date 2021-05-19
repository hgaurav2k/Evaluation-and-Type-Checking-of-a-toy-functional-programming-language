structure TYPE_CHECKER  =
struct
open AST

exception brokenTypes of exp*string*typ*typ

fun match_typ(t1 , t2) = 
  case t1 of 
    INT => (case t2 of INT => true | _ => false)
    |BOOL => (case t2 of BOOL => true | _ => false)
    |ARROW(x,y) => (case t2 of ARROW(z,w) => match_typ(x,z) andalso match_typ(y,w) | _ => false)


fun get_typ(exp , env) : typ = 
  case exp of 
    NumExp(_) => INT
    | Fn(id1,t1,t2,e3) =>( 
                          let 
                            val x = get_typ(e3,envAdd(id1,t1,env))
                          in 
                          if match_typ(x,t2) then ARROW(t1,t2) else raise brokenTypes(Fn(id1,t1,t2,e3),"bad-return-type",t2,x)
                          end 
                          )
    | VarExp(z) => envLookup(z,env)
    | BoolExp(_) => BOOL
    | AppExp ( e1 , e2 ) => 
      let 
        val f = get_typ(e1,env)
        val f2 = get_typ(e2,env)
      in
        case f of ARROW(t1,t2) =>  if match_typ(t1,f2) then t2 else raise brokenTypes(e2,"bad-type", t1, f2)
        | _ => raise brokenTypes(e1 , "not-arrow-type" , f , ARROW(Type_error,Type_error))
      end
    | LetExp(ValDecl(x, e1), e2)  =>
  	let
	    val v1 = get_typ(e1, env) 
	  in
	    get_typ(e2, envAdd (x, v1, env))
    end
    | LetExp(Fun(x,e1) , e2) =>
    let 
      val v1 = get_typ(e1 , env)
    in
      get_typ(e2,envAdd(x,v1,env))
    end		   
    | _ => get_typ_Op(exp , env)


and  get_typ_Op(exp,env) = 
let 
    fun get_typ_UnExp(up , exp , env ) = 
      case up of 
        NEGATE => 
          let val z =  get_typ(exp,env) in 
          (case z of 
             INT => INT
            | _ => raise brokenTypes(exp,"bad-type",INT,z)
          )
          end 

      |NOT => 
          let val z =  get_typ(exp,env) in 
          (case z of 
            BOOL => BOOL
            | _ => raise brokenTypes(exp , "bad-type" , BOOL , z)
          )
        end

    fun get_typ_BinExp(exp1, binop , exp2 , env ) = 
      let
        val e1 = get_typ(exp1 , env) 
        val e2 = get_typ(exp2 , env )
        val e = BinExp(binop,exp1,exp2)
        val exc = brokenTypes(e,"type-mismatch-binop",e1,e2)
      in 
        case binop of 
          PLUS => if e1 = INT andalso e2 = INT then INT else raise exc
          |MINUS => if e1 = INT andalso e2 = INT then INT else raise exc
          |EQUALS => if (e1 = INT andalso e2 = INT) orelse(e1 = BOOL andalso e2 = BOOL) then BOOL else raise exc
          |TIMES => if e1 = INT andalso e2 = INT then INT else raise exc
          |LESSTHAN => if e1 = INT andalso e2 = INT then BOOL else raise exc
          |GREATERTHAN => if e1 = INT andalso e2 = INT then BOOL else raise exc
          |AND => if e1 = BOOL andalso e2 = BOOL then BOOL else raise exc
          |OR => if e1 = BOOL andalso e2 = BOOL then BOOL else raise exc
          |IMPLIES => if e1 = BOOL andalso e2 = BOOL then BOOL else raise exc
          |XOR => if e1 = BOOL andalso e2 = BOOL then BOOL else raise exc
      end
      

    fun get_typ_CondExp(exp1, exp2 , exp3 , env ) = 
      let val t = get_typ(exp1 , env) in 
      case t of
        BOOL => (let 
                  val t1 = get_typ(exp2,env)
                  val t2 = get_typ(exp3,env)
                in 
                  if match_typ(t1,t2) then t1 else raise brokenTypes( CondExp(exp1,exp2,exp3),"type-mismatch" ,t1 , t2)
                end
                )
        |_ => raise brokenTypes(exp1 , "bad-type" , BOOL , t )
      end
in

  case exp of 
    BinExp(binop , exp1 , exp2) => get_typ_BinExp(exp1,binop,exp2,env)
    |UnExp(up1, exp1) =>  get_typ_UnExp(up1 , exp1 , env)
    |CondExp(exp1, exp2 , exp3) => get_typ_CondExp(exp1 , exp2 , exp3 , env)
end




fun handler(e,s,t1,t2) = 
  let 
    val E1 = exptoString(e)
    val T1 = stringtoTyp(t1)
    val T2 = stringtoTyp(t2)
  in

  case s of 
    "bad-return-type" => E1^" expected return type "^T1^" but got "^T2
    |"not-arrow-type" => E1^ " is not of arrow type but of the type "^T1
    |"bad-type" => E1^" expects type "^T1^" but has type "^T2
    |"type-mismatch" => "Types of the operands in "^E1^" should be same but are "^T1^" and "^T2
    |"type-mismatch-binop" => if t1 = INT orelse t1 = BOOL then "Types of the operands in "^E1^" should be same but are "^T1^" and "^T2 
    						   else "Required operation not defined for expression of type "^T1^" in "^E1
  end

fun type_check(program) = 
  let 
    fun f(program , env , l ) = 
    case program of 
      Program(s1,p1) => 
        (case s1 of 
          
               (Expr(exp)) =>  f( p1 ,env , get_typ(exp,env) :: l )
              |(Decl(ValDecl(id,exp))) => f(p1 , envAdd(id,get_typ(exp,env),env) , get_typ(exp,env) :: l )
              |(Decl(Fun(id,Fn(id1,t1,t2,e3)))) => f(p1 , envAdd(id,get_typ(Fn(id1,t1,t2,e3),envAdd(id,ARROW(t1,t2),env)),env) , ARROW(t1,t2) :: l )
        )
      |Statement(Expr(exp)) => get_typ(exp,env) :: l 
      |Statement(Decl(ValDecl(id,exp)))=> get_typ(exp,env) :: l 
      |Statement(Decl(Fun(id,Fn(id1,t1,t2,e3)))) => get_typ(Fn(id1,t1,t2,e3),envAdd(id,ARROW(t1,t2),env)) :: l 
      
  in 
    reverse(f(program , nil , nil ))  
    handle brokenTypes(e,s,t1,t2) => (print(handler(e,s,t1,t2));  [Type_error])
  end

end


