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

-- Lexer

exprDef = emptyDef { identStart = letter
                   , identLetter = alphaNum
                   , reservedNames = ["true", "false"]
                   , opStart = oneOf "+-*/%^!|&<=>"
                   , opLetter = oneOf "+-*/%^!|&<=>"
                   }

lexer :: TokenParser ()
lexer = makeTokenParser exprDef

-- Parsers

parsePredTerm :: Parser PredExpr
parsePredTerm =  parens lexer parsePredExprOps
              <|> parseRealPredTerm
              <|> (reserved lexer "true" >> return (BoolConst True))
              <|> (reserved lexer "false" >> return (BoolConst False))

parseRealPredTerm :: Parser PredExpr
parseRealPredTerm = do
    lVal <- parseRealExprOps
    op <- parseRealPredOperator
    rVal <- parseRealExprOps
    return $ RBinaryExpr op lVal rVal

parseRealPredOperator :: Parser RealPredBinaryOp
parseRealPredOperator =  (reservedOp lexer "<=" >> return RealLessThanEq) 
                     <|> (reservedOp lexer "<" >> return RealLessThan)
                     <|> (reservedOp lexer "==" >> return RealEquals)
                     <|> (reservedOp lexer "!=" >> return RealNotEquals)
                     <|> (reservedOp lexer ">=" >> return RealGreaterThanEq)
                     <|> (reservedOp lexer ">" >> return RealGreaterThan)

parsePredExprOps :: Parser PredExpr
parsePredExprOps = (flip buildExpressionParser) parsePredTerm [
          [ Prefix (reservedOp lexer "!" >> return (PUnaryExpr PredNot))]
        , [ Infix (reservedOp lexer "->" >> return (PBinaryExpr PredImplies)) AssocNone
          , Infix (reservedOp lexer "<->" >> return (PBinaryExpr PredIff)) AssocNone]
        , [ Infix (reservedOp lexer "||" >> return (PBinaryExpr PredOr)) AssocNone
          , Infix (reservedOp lexer "&&" >> return (PBinaryExpr PredAnd)) AssocNone
          , Infix (reservedOp lexer "^^" >> return (PBinaryExpr PredXor)) AssocNone]
        ]

parsePredExpr :: Parser PredExpr
parsePredExpr = do
    whiteSpace lexer
    e <- parsePredExprOps
    eof
    return e

------------------------
-- Numeric real-valued expressions
------------------------

data RealExpr = IntConst Integer 
              | DoubleConst Double
              | VarConst String
              | UnaryExpr RealUnaryOp RealExpr
              | BinaryExpr RealBinaryOp RealExpr RealExpr
    deriving (Show)

data RealUnaryOp = RealNegation | RealPositive
    deriving (Show)

data RealBinaryOp = RealAdd | RealSubtract | RealTimes | RealDivide | RealModulo | RealExponent
    deriving (Show)

-- RealExpr Parsers

parseNumber :: Parser RealExpr
parseNumber = do
    res <- naturalOrFloat lexer
    case res of
        Left nat -> return $ IntConst nat
        Right flt -> return $ DoubleConst flt

parseVar :: Parser RealExpr
parseVar = do
    ident <- identifier lexer
    return $ VarConst ident

parseRealTerm :: Parser RealExpr
parseRealTerm =  parens lexer parseRealExprOps
             <|> parseNumber
             <|> parseVar

parseRealExprOps :: Parser RealExpr
parseRealExprOps = (flip buildExpressionParser) parseRealTerm [
      [ Prefix (reservedOp lexer "-" >> return (UnaryExpr RealNegation))
      , Prefix (reservedOp lexer "+" >> return  (UnaryExpr RealPositive))]
    , [ Infix (reservedOp lexer "**" >> return (BinaryExpr RealExponent)) AssocNone]
    , [ Infix (reservedOp lexer "*" >> return (BinaryExpr RealTimes)) AssocLeft
      , Infix (reservedOp lexer "/" >> return (BinaryExpr RealDivide)) AssocLeft
      , Infix (reservedOp lexer "%" >> return (BinaryExpr RealModulo)) AssocLeft]
    , [ Infix (reservedOp lexer "+" >> return (BinaryExpr RealAdd)) AssocLeft
      , Infix (reservedOp lexer "-" >> return (BinaryExpr RealSubtract)) AssocLeft]
    ]

parseRealExpr :: Parser RealExpr
parseRealExpr = do
    whiteSpace lexer
    e <- parseRealExprOps
    eof
    return e

-- Evaluation

evalRealExpr :: VariableMap -> RealExpr -> Maybe Double
evalRealExpr _ (IntConst i) = Just $ fromIntegral i
evalRealExpr _ (DoubleConst f) = Just f
evalRealExpr m (VarConst varName) = case getVariableMaybe varName m of
                                        Just (IntElement i)     -> evalRealExpr m (IntConst i)
                                        Just (DoubleElement f)  -> evalRealExpr m (DoubleConst f)
                                        Just (StringElement _)  -> Nothing 
                                        Nothing -> Nothing

evalRealExpr m (UnaryExpr RealPositive ex) = evalRealExpr m ex
evalRealExpr m (UnaryExpr RealNegation ex) = do
    exres <- evalRealExpr m ex
    return $ 0 - exres

evalRealExpr m (BinaryExpr op lEx rEx) = do
    lVal <- evalRealExpr m lEx
    rVal <- evalRealExpr m rEx
    case op of
        RealAdd         -> return $ lVal + rVal
        RealSubtract    -> return $ lVal - rVal
        RealTimes       -> return $ lVal * rVal
        RealDivide      -> if rVal == 0 then Nothing else (return $ lVal / rVal)
        RealModulo      -> undefined
        RealExponent    -> return $ lVal ** rVal