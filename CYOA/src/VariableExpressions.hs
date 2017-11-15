module VariableExpressions (evalExprInt, evalExprDouble) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Text.Parsec.Token

import VariableData

evalExprInt :: String -> Integer
evalExprInt str = undefined

evalExprDouble :: String -> Double
evalExprDouble str = undefined

------------------------
-- Boolean-valued predicate expressions
------------------------

data PredExpr = BoolConst Bool
              | RBinaryExpr RealPredBinaryOp RealExpr RealExpr
              | PUnaryExpr PredUnaryOp PredExpr
              | PBinaryExpr PredBinaryOp PredExpr PredExpr
    deriving (Show)

data RealPredBinaryOp = RealLessThan | RealLessThanEq | RealEquals | RealNotEquals | RealGreaterThanEq | RealGreaterThan
    deriving (Show)

data PredUnaryOp = PredNot
    deriving (Show)

data PredBinaryOp = PredAnd | PredOr | PredXor | PredImplies | PredIff
    deriving (Show)

------------------------
-- Numeric real-valued expressions
------------------------

data RealExpr = IntConst Integer 
              | DoubleConst Double
              | VarConst String
              | ParenElem RealExpr
              | UnaryExpr RealUnaryOp RealExpr
              | BinaryExpr RealBinaryOp RealExpr RealExpr
    deriving (Show)

data RealUnaryOp = RealNegation | RealPositive
    deriving (Show)

data RealBinaryOp = RealAdd | RealSubtract | RealTimes | RealDivide | RealModulo | RealExponent
    deriving (Show)

realDef = emptyDef { identStart = letter
                   , identLetter = alphaNum
                   , opStart = oneOf "+-*/%^"
                   , opLetter = oneOf "+-*/%^"
                   }

-- Lexer

realLexer :: TokenParser ()
realLexer = makeTokenParser realDef

-- Parsers

parseNumber :: Parser RealExpr
parseNumber = do
    res <- naturalOrFloat realLexer
    case res of
        Left nat -> return $ IntConst nat
        Right flt -> return $ DoubleConst flt

parseVar :: Parser RealExpr
parseVar = do
    ident <- identifier realLexer
    return $ VarConst ident

parseRealTerm :: Parser RealExpr
parseRealTerm =  parens realLexer parseRealExprOps
             <|> parseNumber
             <|> parseVar

parseRealExprOps :: Parser RealExpr
parseRealExprOps = (flip buildExpressionParser) parseRealTerm [
      [ Prefix (reservedOp realLexer "-" >> return (UnaryExpr RealNegation))
      , Prefix (reservedOp realLexer "+" >> return  (UnaryExpr RealPositive))]
    , [ Infix (reservedOp realLexer "^" >> return (BinaryExpr RealExponent)) AssocNone]
    , [ Infix (reservedOp realLexer "*" >> return (BinaryExpr RealTimes)) AssocLeft
      , Infix (reservedOp realLexer "/" >> return (BinaryExpr RealDivide)) AssocLeft
      , Infix (reservedOp realLexer "%" >> return (BinaryExpr RealModulo)) AssocLeft]
    , [ Infix (reservedOp realLexer "+" >> return (BinaryExpr RealAdd)) AssocLeft
      , Infix (reservedOp realLexer "-" >> return (BinaryExpr RealSubtract)) AssocLeft]
    ]

parseRealExpr :: Parser RealExpr
parseRealExpr = do
    whiteSpace realLexer
    e <- parseRealExprOps
    eof
    return e