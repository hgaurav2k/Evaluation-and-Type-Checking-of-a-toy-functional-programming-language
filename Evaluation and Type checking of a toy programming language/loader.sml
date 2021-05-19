CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");
use "ast.sml";
use "calc.yacc.sig";
use "calc.yacc.sml";
use "calc.lex.sml";
use "load-calc.sml";
use "evaluator.sml";
use "type-checker.sml";
open AST ; 
open EVALUATOR ; 
open TYPE_CHECKER  ;
use "a3.sml" ;
Control.Print.printLength := 1000; (* set printing parameters so that *)
Control.Print.printDepth := 1000; (* we’ll see all details *)
Control.Print.stringDepth := 1000; (* and strings *)
