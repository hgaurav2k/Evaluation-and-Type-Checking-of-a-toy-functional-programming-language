structure Tokens= Tokens
  
  type pos = int
  type svalue = Tokens.svalue
  type ('a,'b) token = ('a,'b) Tokens.token  
  type lexresult = (svalue, pos) token

  val linenum = ref 1
  val colnum = ref 0 
  val flag = ref false
  val error = fn x => TextIO.output(TextIO.stdOut,x ^ "\n")
  val eof = fn () => if !flag then OS.Process.exit(OS.Process.success) else Tokens.EOF(!linenum, !colnum)
  fun refinc x  =  (x := !x + 1; !x)
  exception UnknownToken of int*int*string ;

  fun revfold _ nil b = b
  | revfold f (hd::tl) b = revfold f tl (f(hd,b))


  val keywords =
  [
   ("PLUS",  Tokens.PLUS),
   ("LESSTHAN" , Tokens.LESSTHAN),
   ("GREATERTHAN" , Tokens.GREATERTHAN),
   ("MINUS", Tokens.MINUS),
   ("TIMES", Tokens.TIMES),
   ("XOR" , Tokens.XOR),
   ("AND" , Tokens.AND),
   ("OR" , Tokens.OR),
   ("IMPLIES" , Tokens.IMPLIES),
   ("NOT" , Tokens.NOT),
    ("NEGATE" , Tokens.NEGATE),
    ("EQUALS" , Tokens.EQUALS),
   ("end",  Tokens.END),
   ("in",  Tokens.IN),
   ("let",  Tokens.LET),
   ("if",  Tokens.IF),
   ("then" , Tokens.THEN),
   ("else" , Tokens.ELSE),
   ("fi" , Tokens.FI),
   ("int" , Tokens.INT),
   ("bool" , Tokens.BOOL),
   ("fun" , Tokens.FUN),
   ("fn" , Tokens.FN)
   ]

  fun findKeywords (str:string, pos1:pos, pos2:pos) =
  if str = "TRUE"  then Tokens.CONST(true,pos1,pos2) 
  else if str = "FALSE" then Tokens.CONST(false,pos1,pos2)
  else
    (case List.find (fn (s, _) => s = str )  keywords of
    SOME (_, tk) => tk(pos1, pos2) 
    | NONE => Tokens.ID (str, pos1, pos2))

%%
%header (functor CalcLexFun(structure Tokens:Calc_TOKENS));

alpha=[A-Za-z];
all =[A-Za-z0-9];
digit=[0-9];
ws = [\ \t];
%%
\n       => (colnum := 0 ; linenum := (!linenum) + 1; lex());
\r\n       => (colnum := 0 ; linenum := (!linenum) + 1; lex());
";"      => (colnum := (!colnum) + 1 ; Tokens.TERM(!linenum,!colnum));
{ws}+    => (colnum := (!colnum) + size yytext ; lex());
{digit}+ => (colnum := (!colnum) + size yytext ; Tokens.NUM
	     (List.foldl (fn (a,r) => ord(a) - ord(#"0") + 10*r) 0 (explode yytext),
	      !linenum, !colnum));
"->"     => (colnum := (!colnum) + 1 ;Tokens.ARROW(!linenum,!colnum));
":"      => (colnum := (!colnum) + 1 ;Tokens.COLON(!linenum,!colnum));
"("      => (colnum := (!colnum) + 1 ;Tokens.LPAREN(!linenum,!colnum));
")"      => (colnum := (!colnum) + 1 ;Tokens.RPAREN(!linenum,!colnum));
"=>"     => (colnum := (!colnum) + 1 ;Tokens.GIVES(!linenum,!colnum));
{alpha}{all}* => (colnum := (!colnum) + size yytext ; findKeywords(yytext,!linenum,!colnum));
"="      => (colnum := (!colnum) + 1 ;Tokens.EQ(!linenum,!colnum)); 
. => (refinc colnum ; (flag := true ) ; error("Unknown Token:"^Int.toString(!linenum)^":"^Int.toString(!colnum)^":"^yytext) ; lex());
