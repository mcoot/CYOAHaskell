module VariableExpressions (evalRealExpr, evalPredExpr) where

import Data.Fixed (mod')

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Text.Parsec.Token

import Lib
import VariableData

evalRealExpr :: VariableMap -> String -> Maybe Double
evalRealExpr m str = do
    parseRes <- parseRealExprMaybe str
    evalRealExprAST m parseRes
    
    
evalPredExpr :: VariableMap -> String -> Maybe Bool
evalPredExpr m str = do
    parseRes <- parsePredExprMaybe str
    evalPredExprAST m parseRes   

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

parsePredExprMaybe :: String -> Maybe PredExpr
parsePredExprMaybe str = case parse parsePredExpr "" str of
                            Left _      -> Nothing
                            Right ex    -> Just ex

-- Evaluation

evalPredExprAST :: VariableMap -> PredExpr -> Maybe Bool
evalPredExprAST _ (BoolConst b) = return b

evalPredExprAST m (RBinaryExpr op lEx rEx) = do
    lVal <- evalRealExprAST m lEx
    rVal <- evalRealExprAST m rEx
    case op of
        RealLessThan -> return $ lVal < rVal
        RealLessThanEq -> return $ lVal <= rVal
        RealEquals -> return $ lVal == rVal
        RealNotEquals -> return $ lVal /= rVal
        RealGreaterThanEq -> return $ lVal >= rVal
        RealGreaterThan -> return $ lVal > rVal

evalPredExprAST m (PUnaryExpr PredNot ex) = do
    exres <- evalPredExprAST m ex
    return $ not exres

evalPredExprAST m (PBinaryExpr op lEx rEx) = do
    lVal <- evalPredExprAST m lEx
    rVal <- evalPredExprAST m rEx
    case op of
        PredAnd -> return (lVal && rVal)
        PredOr -> return (lVal || rVal)
        PredXor -> return (lVal `xor` rVal)
        PredImplies -> return (lVal `implies` rVal)
        PredIff -> return (lVal `iff` rVal)
        
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

parseRealExprMaybe :: String -> Maybe RealExpr
parseRealExprMaybe str = case parse parseRealExpr "" str of
                            Left _      -> Nothing
                            Right expr  -> Just expr

-- Evaluation

evalRealExprAST :: VariableMap -> RealExpr -> Maybe Double
evalRealExprAST _ (IntConst i) = return $ fromIntegral i
evalRealExprAST _ (DoubleConst f) = return f
evalRealExprAST m (VarConst varName) = case getVariableMaybe varName m of
                                        Just (IntElement i)     -> evalRealExprAST m (IntConst i)
                                        Just (DoubleElement f)  -> evalRealExprAST m (DoubleConst f)
                                        Just (StringElement _)  -> Nothing 
                                        Nothing -> Nothing

evalRealExprAST m (UnaryExpr RealPositive ex) = evalRealExprAST m ex
evalRealExprAST m (UnaryExpr RealNegation ex) = do
    exres <- evalRealExprAST m ex
    return $ 0 - exres

evalRealExprAST m (BinaryExpr op lEx rEx) = do
    lVal <- evalRealExprAST m lEx
    rVal <- evalRealExprAST m rEx
    case op of
        RealAdd         -> return $ lVal + rVal
        RealSubtract    -> return $ lVal - rVal
        RealTimes       -> return $ lVal * rVal
        RealDivide      -> if rVal == 0 then Nothing else (return $ lVal / rVal)
        RealModulo      -> return $ mod' lVal rVal
        RealExponent    -> return $ lVal ** rVal

