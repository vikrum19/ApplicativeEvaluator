{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Data.Bool (bool)
import Data.Char
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f p = P $ \s -> [(s', f a) | (s', a) <- unP p s]

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) = pmap . const

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p =
  P $ \s ->
    case s of
      []   -> []
      x:xs -> bool [] [(xs, x)] (p x)

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \s -> [(s, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px =
  P $ \s -> do
    (s', f) <- unP pf s
    (s'', x) <- unP px s'
    return (s'', f x)

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = inject const <@> pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = inject (flip const) <@> pa <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP s = foldl (\p c -> p <@ charP c) (inject s) s

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
(<<>>) px py = P $ \s -> unP px s ++ unP py s

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = inject [] <<>> some p

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = inject (:) <@> p <@> many p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = [a | ("", a) <- unP p cs]

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs =
  case runParser p cs of
    [a] -> Just a
    _   -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE x) = x
evalExpr (BinOpE op e1 e2) =
  case op of
    AddBO -> evalExpr e1 + evalExpr e2
    MulBO -> evalExpr e1 * evalExpr e2

evalExpr (NegE e) = negate . evalExpr $ e

evalExpr ZeroE = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
--

parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique expr

  where
    expr = int <<>> binOpExpr <<>> neg <<>> zero

    int = ConstE . read <#> some (predP isDigit)

    neg = NegE <#> (charP '-' @> expr)

    zero = ZeroE <# charP 'z'

    binOpExpr = charP '(' @> (add <<>> mul) <@ charP ')'

    add = BinOpE AddBO <#> expr <@ stringP " + " <@> expr

    mul = BinOpE MulBO <#> expr <@ stringP " * " <@> expr


main :: IO ()
main = do
  putStrLn "Enter an arithmetic expression:"
  input <- getLine
  case parseExpr input of
    Just expr -> do
      putStrLn $ "Parsed expression: " ++ show expr
      putStrLn $ "Result of evaluation: " ++ show (evalExpr expr)
    Nothing -> putStrLn "Failed to parse expression."
