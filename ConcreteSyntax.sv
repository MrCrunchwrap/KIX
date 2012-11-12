grammar kix ;

{- This file defines the concrete syntax of "kix".  
   The concrete productions are used to define the
   parser.  They also define an attribute, pp, which unparses the tree
   to generate a string, and attributes, ast_Root and ast_Expr, which
   are used to generate the abstract syntax tree.  -}

nonterminal Root_c with pp, ast_Root;

synthesized attribute ast_Root :: Root;
synthesized attribute ast_DeclList :: DeclList;
synthesized attribute ast_Dec :: Dec;
synthesized attribute ast_Input :: Input;
synthesized attribute ast_Stmts :: Stmts ;
synthesized attribute ast_Stmt :: Stmt ;
synthesized attribute ast_Expr :: Expr ;
synthesized attribute ast_Decl :: Decl ;
synthesized attribute ast_TypeExpr :: TypeExpr ;

concrete production root_c
r::Root_c ::= dl::DeclList_c
{
  r.pp = dl.pp ;
  r.ast_Root = root(dl.ast_DeclList);
}

-- DeclList
nonterminal DeclList_c with pp, ast_DeclList ;

concrete production oneDecl_c
dl::DeclList_c ::= d::Dec_c
{
	dl.pp = d.pp;
	dl.ast_DeclList = oneDecl(d.ast_Dec);
}

concrete production decls_c
dl::DeclList_c ::= d::Dec_c rest::DeclList_c
{
	dl.pp = d.pp ++ rest.pp;
	dl.ast_DeclList = decls(d.ast_Dec, rest.ast_DeclList);
}

-- Dec
nonterminal Dec_c with pp, ast_Dec ;

concrete production mainDecl_c
d::Dec_c ::= 'main' '(' ')' '{' ss::Stmts_c '}'
{
	d.pp = "main(){ " ++ ss.pp ++ " }";
	d.ast_Dec = mainDecl(ss.ast_Stmts);
}

concrete production funcDecl_c
d::Dec_c ::= 'Function' te1::TypeExpr_c v1:: VariableName '(' input::Input_c ')' '{' ss::Stmts_c '}'
{
	d.pp = "Function " ++ te1.pp ++ " " ++ v1.lexeme ++ "(" ++ input.pp ++ ") { " ++ ss.pp ++ " }";
	d.ast_Dec = funcDecl(te1.ast_TypeExpr,v1,input.ast_Input,ss.ast_Stmts);
}

nonterminal Input_c with pp, ast_Input ;

concrete production inputList_c
input::Input_c ::= te::TypeExpr_c v:: VariableName ',' rest::Input_c
{
	input.pp = te.pp ++ " " ++ v.lexeme ++ ", " ++ rest.pp;
	input.ast_Input = inputList(te.ast_TypeExpr,v,rest.ast_Input);
}

concrete production inputOne_c
input::Input_c ::= te::TypeExpr_c v::VariableName
{
	input.pp = te.pp ++ " " ++ v.lexeme;
	input.ast_Input = inputOne(te.ast_TypeExpr,v);
}

-- Stmts
nonterminal Stmts_c with pp, ast_Stmts ;

concrete production consStmts_c
ss::Stmts_c ::= s::Stmt_c  rest::Stmts_c
{
  ss.pp = s.pp ++ rest.pp ;
  ss.ast_Stmts = consStmts ( s.ast_Stmt, rest.ast_Stmts ) ;
}

concrete production nilStmts_c
ss::Stmts_c ::= 
{
  ss.pp = "" ;
  ss.ast_Stmts = nilStmts ( );
}

-- Stmt
nonterminal Stmt_c with pp, ast_Stmt ;

concrete production declStmt_c
s::Stmt_c ::= d::Decl_c ';'
{
  s.pp = d.pp ++ ";";
  s.ast_Stmt = declStmt(d.ast_Decl) ;
}

{-
abstract production block_c
s::Stmt_c ::= '{' ss::Stmts_c  '}'
{
}
-}

concrete production assignmentStmt_c
s::Stmt_c ::= l::Expr_c '=' r::Expr_c ';'
{
  s.pp = l.pp ++ " = " ++ r.pp ++ ";";
  s.ast_Stmt = assignmentStmt(l.ast_Expr, r.ast_Expr) ;
}

concrete production returnStmt_c
s::Stmt_c ::= 'return' e::Expr_c ';'
{
	s.pp = "return " ++ e.pp ++ " ;";
	s.ast_Stmt = returnStmt(e.ast_Expr);
}

concrete production printStmt_c
ps::Stmt_c ::= 'print' '(' e::Expr_c ')' ';'
{
  ps.pp = "print ( " ++ e.pp ++ " ) ;" ;
  ps.ast_Stmt = printStmt(e.ast_Expr) ;
}

concrete production ifThenElseStmt_c
s::Stmt_c ::= 'if' '(' e::Expr_c ')' th::Stmt_c 'else' el::Stmt_c
{
  s.pp = "if (" ++ e.pp ++ ") " ++ th.pp ++ " else " ++ el.pp;
  s.ast_Stmt = ifThenElseStmt(e.ast_Expr, th.ast_Stmt, el.ast_Stmt) ;
}

-- Decl

nonterminal Decl_c with pp, ast_Decl ;

concrete production decl_c
d::Decl_c ::= te::TypeExpr_c n::VariableName
{
  d.pp = te.pp ++ " " ++ n.lexeme ;
  d.ast_Decl = decl(te.ast_TypeExpr, n) ;
}

-- TypeExpr

nonterminal TypeExpr_c with pp, ast_TypeExpr ;

concrete production intTypeExpr_c
te::TypeExpr_c ::= i::Integer_t
{
  te.pp = i.lexeme ;
  te.ast_TypeExpr = intTypeExpr(i);
}

concrete production boolTypeExpr_c
te::TypeExpr_c ::= b::Boolean_t
{
  te.pp = b.lexeme ;
  te.ast_TypeExpr = boolTypeExpr(b);
}

-- Expr

nonterminal Expr_c with pp, ast_Expr ;

concrete production add_c
e::Expr_c ::= e1::Expr_c '+' e2::Expr_c
{
	e.pp = e1.pp ++ " + " ++ e2.pp;
	e.ast_Expr = add(e1.ast_Expr,e2.ast_Expr);
}

concrete production sub_c
e::Expr_c ::= e1::Expr_c '-' e2::Expr_c
{
	e.pp = e1.pp ++ " - " ++ e2.pp;
	e.ast_Expr = sub(e1.ast_Expr,e2.ast_Expr);
}

concrete production mult_c
e::Expr_c ::= e1::Expr_c '*' e2::Expr_c
{
	e.pp = e1.pp ++ " * " ++ e2.pp;
	e.ast_Expr = mult(e1.ast_Expr,e2.ast_Expr);
}

concrete production div_c
e::Expr_c ::= e1::Expr_c '/' e2::Expr_c
{
	e.pp = e1.pp ++ " / " ++ e2.pp;
	e.ast_Expr = div(e1.ast_Expr,e2.ast_Expr);
}

concrete production mod_c
e::Expr_c ::= e1::Expr_c '%' e2::Expr_c
{
	e.pp = e1.pp ++ " % " ++ e2.pp;
	e.ast_Expr = mod(e1.ast_Expr,e2.ast_Expr);
}

concrete production lessThan_c
e::Expr_c ::= e1::Expr_c '<' e2::Expr_c
{
	e.pp = e1.pp ++ " < " ++ e2.pp;
	e.ast_Expr = lessThan(e1.ast_Expr,e2.ast_Expr);
}

concrete production greaterThan_c
e::Expr_c ::= e1::Expr_c '>' e2::Expr_c
{
	e.pp = e1.pp ++ " > " ++ e2.pp;
	e.ast_Expr = greaterThan(e1.ast_Expr,e2.ast_Expr);
}

concrete production greaterThanEq_c
e::Expr_c ::= e1::Expr_c '>=' e2::Expr_c
{
	e.pp = e1.pp ++ " >= " ++ e2.pp;
	e.ast_Expr = greaterThanEq(e1.ast_Expr,e2.ast_Expr);
}

concrete production lessThanEq_c
e::Expr_c ::= e1::Expr_c '<=' e2::Expr_c
{
	e.pp = e1.pp ++ " <= " ++ e2.pp;
	e.ast_Expr = lessThanEq(e1.ast_Expr,e2.ast_Expr);
}

concrete production notEq_c
e::Expr_c ::= e1::Expr_c '!=' e2::Expr_c
{
	e.pp = e1.pp ++ " != " ++ e2.pp;
	e.ast_Expr = notEq(e1.ast_Expr,e2.ast_Expr);
}


concrete production varName_c
e::Expr_c ::= n::VariableName
{
  e.pp = n.lexeme ;
  e.ast_Expr = varName(n);
}

concrete production intLit_c
e::Expr_c ::= i::IntegerLiteral
{
  e.pp = i.lexeme ;
  e.ast_Expr = intLit(i);
}

concrete production boolLit_c
e::Expr_c ::= b::BooleanLiteral
{
  e.pp = b.lexeme ;
  e.ast_Expr = boolLit(b);
}
