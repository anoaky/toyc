# Parser Grammar

Details the language recognized by the parser, in EBNF, per ISO 14977 (more or less). This grammar should be `LL(k)`.

## Basic definitions (see [Lexer](crate::lexer::Token))

Definitions of `alpha-lower`, `alpha-upper`, and `digit` are omitted. You know what uppercase letters, lowercase letters, and digits are, I hope.
~~~text
nat = digit , { digit } ;
int = [ "-" ] , nat ;
alpha = alpha-lower | alpha-upper ;
alphanum = alpha-lower | alpha-upper | digit ;
ident =  alpha , { "_" | alphanum } | "_" , ( "_" | alphanum ) , { "_" | alphanum };
special-character = (* See src/lexer.rs. I'm not typing them all out *) ;
escaped-character = "\" , ( '"' | "'" | "n" | "t" | "r" | ( "x" , 2 * digit ) ) ;
~~~

## [Items](crate::ast::Item)
~~~text
item = static-decl | struct-decl | fn-decl | fn-defn ;
static-var = "static" , ident , [ ":" , type ] , [ "=" , literal ] , ";" ;
struct-field = ident , ":" , strict-type , ";" ;
struct-decl = "struct" , ident , "{" , { struct-field } , "}" ;
fn-params = [ ident , ":" , strict-type , { "," , ident , ":" , strict-type } ] ;
fn-decl = ident , "(" , fn-params , ")" , ":" , strict-type , ";" ;
fn-defn = ident , "(" , fn-params , ")" , ":" , strict-type ,  block ; 
~~~

## [Statements](crate::ast::statements::Stmt)
~~~text
stmt = block | local-var | while | if | expr-stmt | continue | break ;
block = "{" , { stmt } , "}" ; 
local-var-decl = "let" , ident , ":" , strict-type , ";" ;
local-var-defn = "let" , ident , ":" , type , "=" , expr , ";" ;
local-var-quick-defn = "let" , ident , ":=" , expr , ";" ;
while = "while" , "(" , expr , ")" , stmt ;
for = "for" , "(" , range_pattern , ")" , stmt ;
if = "if" , "(" , expr , ")" , stmt , [ "else" , stmt ] ;
expr-stmt = expr , [ ";" ] ;
continue = "continue;" ;
break = "break;" ;
~~~

## Patterns
~~~text
range_pattern = ident , ":" , ( "[" | "(" ) , int, "," , int , ( "]" | ")" ) ;
~~~

## [Types](crate::ast::types::Ty)
~~~text
type = base-type , type' | "&" , type | "(" , type , ")" , type' ;
strict-type = ( base-type - "_" ) , type' | "&" , strict-type | "(" , strict-type , ")" , type' ;
base-type = "int" | "char" | "struct" , ident | "_" ;
type' = [ "[" , nat , "]" , type' ] ;
~~~

## [Expressions](crate::ast::exprs::Expr)
~~~text
WIP (needs left-factoring)
~~~

### [Literals](crate::ast::exprs::Literal)
~~~text
literal = int-literal | char-literal | str-literal ;
int-literal = nat ; (* negative numbers are implemented as a binary operation *)
char-literal = "'" , ( alphanum | special | '"' | " " | escaped-character ) , "'" ;
str-literal = '"' , { alphanum | special | "'" | " " | escaped-character }, '"' ;
~~~
