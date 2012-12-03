grammar kix ;

{- The abstract syntax is defined here.  Defining abstract syntax is
   not required in Silver since attributes can decorate the concrete
   syntax as well.  But in many cases, especially when the concrete
   syntax is complicated by parsing requirements, it is useful to do
   so.  -}

{- The "pretty print" of a tree. Should parse to the "same" tree. -}
synthesized attribute pp :: String;

synthesized attribute errors :: [ String ] ;

synthesized attribute html :: String;

inherited attribute env :: [ Pair<String   Decorated Decl> ] ;
synthesized attribute defs :: [ Pair<String    Decorated Decl> ] ;

nonterminal Root with pp, html, errors;

abstract production root
r::Root ::= dl::DeclList
{
	r.pp = dl.pp ;
	r.errors = dl.errors ;
	r.html = dl.html;
	

}

nonterminal DeclList with pp, html, errors;

abstract production oneDecl
dl::DeclList ::= d::Dec
{
	dl.pp = d.pp;
	dl.errors = d.errors;
	dl.html = d.html;
}

abstract production decls
dl::DeclList ::= d::Dec  rest::DeclList
{
	dl.pp = d.pp ++ rest.pp;
	dl.errors = d.errors ++ rest.errors;
	dl.html = d.html ++ rest.html;
}

nonterminal Dec with pp, html, errors;

abstract production mainDecl
d::Dec ::= ss::Stmts
{
	d.pp = "main(){ " ++ ss.pp ++ " }";
	d.errors = ss.errors;
	d.html = ss.html;
	ss.env = [];
}

abstract production funcDocDec
d::Dec ::= fd::Dec
{
	d.pp = "Function documentation";
	d.errors = fd.errors;
	d.html = fd.html;
}


abstract production funcDecl
d::Dec ::= te1::TypeExpr v1::VariableName input::Input ss::Stmts
{
	d.pp = "Function " ++ te1.pp ++ " " ++ v1.lexeme ++ "(" ++ input.pp ++ ") { " ++ ss.pp ++ " }";
	d.errors = ss.errors;
	ss.env = [];
	d.html = "<p> Test function </p>";
}

nonterminal Input with pp, html, errors;

abstract production inputList
input::Input ::= te::TypeExpr v::VariableName rest::Input
{
	input.pp = te.pp ++ " " ++ v.lexeme ++ ", " ++ rest.pp;
	input.html = te.html ++ rest.html;
}

abstract production inputOne
input::Input ::= te::TypeExpr v::VariableName
{
	input.pp = te.pp ++ " " ++ v.lexeme;
	input.html = te.html;
}

nonterminal Stmts with pp, html, errors, env ;
abstract production consStmts
ss::Stmts ::= s::Stmt rest::Stmts
{
 	ss.pp = s.pp ++ rest.pp ;
 	ss.errors = s.errors ++ rest.errors ;
 	s.env = ss.env ;
 	rest.env = s.defs ++ ss.env ;
	ss.html = s.html ++ rest.html;
}

abstract production nilStmts
ss::Stmts ::= 
{
	ss.pp = "" ;
	ss.errors = [ ] ;
	ss.html = "";
}

nonterminal Stmt with pp, html, errors, env, defs;

abstract production declStmt
s::Stmt ::= d::Decl
{
	s.pp = d.pp ++ ";" ;
	s.errors = [ ] ; --TODO fix this later
	s.defs = d.defs ;
	s.html = d.html;
}

abstract production exprStmt
s::Stmt ::= e1::Expr e2::Expr
{
	s.pp = e1.pp ++ " = " ++ e2.pp ++ ";";
	s.errors = [ ] ; --TODO fix this later
	s.html = e1.html ++ e2.html;
}


abstract production block
s::Stmt ::= ss::Stmts 
{
}


abstract production assignmentStmt
s::Stmt ::= l::Expr r::Expr
{
  s.pp = l.pp ++ " = " ++ r.pp ++ ";";
  s.errors = l.errors ++ r.errors ;
  s.defs = [ ] ;
  l.env = s.env ;
  r.env = s.env ;
	s.html = l.html ++ r.html;
}

abstract production returnStmt
s::Stmt ::= e::Expr
{
	s.pp = "return " ++ e.pp ++ " ;";
	s.errors = e.errors ;
	s.defs = [ ] ;
	e.env = s.env ;
	s.html = e.html;
}

abstract production printStmt
s::Stmt ::= e::Expr
{
  s.pp = "print ( " ++ e.pp ++ " ) ;" ;
  s.errors = e.errors ;
  s.defs = [ ] ;
  e.env = s.env ;
	s.html = e.html;
}

abstract production ifThenElseStmt
s::Stmt ::= e::Expr th::Stmt el::Stmt
{
  s.pp = "if (" ++ e.pp ++ ") " ++ th.pp ++ " else " ++ el.pp ;
  s.errors = e.errors ++ th.errors ++ el.errors ;
  s.defs = [ ] ;
  e.env = s.env ;
  th.env = s.env ;
  el.env = s.env ;
	s.html = "";
}

nonterminal Decl with pp, html, defs ;
abstract production decl
d::Decl ::= te::TypeExpr n::VariableName
{
  d.pp = te.pp ++ " " ++ n.lexeme ;
  d.defs = [ pair(n.lexeme, d) ] ;
	d.html = te.html;
}

nonterminal TypeExpr with pp, html;

abstract production intTypeExpr
te::TypeExpr ::= i::Integer_t
{
  te.pp = i.lexeme ;
	te.html = "";
}

abstract production boolTypeExpr
te::TypeExpr ::= b::Boolean_t
{
  te.pp = b.lexeme ;
	te.html = "";
}

nonterminal Expr with pp, html, errors, env;

abstract production add
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " + " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production sub
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " - " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production mult
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " * " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production div
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " / " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production mod
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " % " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production lessThan
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " < " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production greaterThan
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " > " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production greaterThanEq
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " >= " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production lessThanEq
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " <= " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}

abstract production notEq
e::Expr ::= e1::Expr e2::Expr
{
	e.pp = e1.pp ++ " != " ++ e2.pp;
	e.errors = [ ] ; --TODO fix this later
	e.html = "";
}


abstract production varName
e::Expr ::= n::VariableName
{
  e.pp = n.lexeme ;
  e.errors = case lookup (n.lexeme, e.env) of
               nothing() -> [ n.lexeme ++ " is not defined. \n\n" ] 
             | just(d) -> [ ]
             end ;
	e.html = "";
}

function lookup
Maybe<Decorated Decl> ::= name::String env::[ Pair<String   Decorated Decl>]
{
  return if null(env)
         then nothing() 
         else if name == (head(env)).fst
              then just((head(env)).snd)
              else lookup (name, tail(env)) ;
}

abstract production intLit
e::Expr ::= i::IntegerLiteral
{
  e.pp = i.lexeme ;
  e.errors = [ ] ;
	e.html = "";
}

abstract production boolLit
e::Expr ::= b::BooleanLiteral
{
  e.pp = b.lexeme ;
  e.errors = [ ] ;
	e.html = "";
}
