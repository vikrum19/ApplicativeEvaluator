# ApplicativeEvaluator
An applicative parser and evaluator to parse and evaluate expressions from scratch

What is a parser?
Parsers find structure within text, either to validate, or to get text into some representation. 
For example, in case we want to parse an integer from a string, we expect a value of an integer type as result of the parser. 
Therefore we parametrize our parser over the type of the expected value, this leads to the following definition.

newtype Parser a = P { unP :: String -> [(String, a)] }
It's just a function of the input string, returning a list of possible outcomes! A single outcome is the remaining input and the parsed value. 
Why multiple outcomes? Well, we could parse things in different ways. For example "1 + 2 + 3" could be parsed as (1 + 2) + 3 or 1 + (2 + 3).


Given an expression datatype Expr for arithmetic expressions on integers, write functions to parse and evaluate expressions. The following grammar will be used (no random whitespaces are allowed):

expr         ::= const | binOpExpr | neg | zero
const        ::= int
binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
binOp        ::= '+' | '*'
neg          ::= '-' expr
zero         ::= 'z'
int          ::= digit +
digit        ::= '0' | ... | '9'
The expression type:

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)
          
Examples on how parseExpr should work:

parseExpr "(z + 1)"         == Just (BinOpE AddBO ZeroE (ConstE 1))
parseExpr " (z + z)"        == Nothing
parseExpr "(z+1) "          == Nothing
parseExpr "-((z * 2) + -1)" == Just (NegE (BinOpE AddBO (BinOpE MulBO ZeroE (ConstE 2)) (NegE (ConstE 1))))

Omitted the use of Control.Applicative and Data.Functor.
