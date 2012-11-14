grammar kix;

{- Ignore terminals define the whitespace for the grammar, as a whole.
 - We also typically include comments
 -}
ignore terminal WhiteSpace /[\t\n\ ]+/;
ignore terminal LineComment /\/\/.*/;

-- Operator precedence and associativity can be specified directly.
-- Higher values indicate higher precedence operators.
-- Associativity can be set to "left" or "right", default is non

terminal Star    '*'   precedence = 12, association = left;
terminal Slash   '/'   precedence = 12, association = left;
terminal Percent '%'   precedence = 12, association = left;

terminal Plus   '+'   precedence = 10, association = left;
terminal Dash   '-'   precedence = 10, association = left;

terminal EqEq   '=='  precedence =  8;
terminal NEq    '!='  precedence =  8;
terminal Lt     '<'   precedence =  8;
terminal LtEq   '<='  precedence =  8;
terminal Gt     '>'   precedence =  8;
terminal GtEq   '>='  precedence =  8;

terminal Eq     '=';

-- Punctuation

terminal LeftParen  '('  precedence = 16 ;
terminal RightParen ')' precedence= 16;
terminal LeftCurly  '{';
terminal RightCurly '}';
terminal Semicolon  ';';
terminal Comma		',';

lexer class KEYWORDS;

-- Statements

terminal If      'if'     lexer classes { KEYWORDS }; 
terminal Else    'else'   lexer classes { KEYWORDS };
terminal Return  'return' lexer classes { KEYWORDS };
terminal Print   'print'  lexer classes { KEYWORDS };
terminal Main	 'main'   lexer classes { KEYWORDS };
terminal Func	 'Function'   lexer classes { KEYWORDS };

-- Doc Comment
terminal BeginDoc '**/';
terminal EndDoc '/**';

-- Types
terminal Integer_t 'Integer' lexer classes { KEYWORDS };
terminal Boolean_t 'Boolean' lexer classes { KEYWORDS };

-- Expressions

--- Used in:
---   Decl ::= TypeExpr variableName ';'
---   Expr ::= variableName
terminal VariableName  /[a-zA-Z][a-zA-Z0-9_]*/  submits to { KEYWORDS };

terminal StringLiteral /\'([^\']|\'\')*\'/;

--- Used in:
---   Expr ::= integerLiteral
terminal IntegerLiteral /[0-9]+/;

--- Used in:
---   Expr ::= booleanLiteral
terminal BooleanLiteral /(True)|(False)/ lexer classes { KEYWORDS };


