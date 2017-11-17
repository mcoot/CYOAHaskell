module LineRendering (renderVarsText) where

import Data.List
import Text.Regex.PCRE
import Data.String.Utils

import Lib
import VariableData
import VariableExpressions

-- | Render variables and expression literals into a string
renderVarsText :: VariableMap -> String -> String
renderVarsText varMap = (replaceExpressionsInText varMap) . (replaceVarsInText varMap) 

------------------------
-- Render variables in text
------------------------

renderVariableElement :: VariableElement -> String
renderVariableElement (IntElement val) = show val
renderVariableElement (StringElement val) = val
renderVariableElement (DoubleElement val) = show val

getMatches :: String -> String -> [String]
getMatches str pattern = getAllTextMatches (str =~ pattern :: AllTextMatches [] String)

getVarsTextVariablesBracketed :: String -> [String]
getVarsTextVariablesBracketed str = map (init . drop 2) (getMatches str "\\${[A-Za-z0-9]+}")

getVarsTextVariablesUnbracketed :: String -> [String]
getVarsTextVariablesUnbracketed str = map (drop 1)  (getMatches str "\\$[A-Za-z0-9]+")

getVarsTextVariables :: String -> [String]
getVarsTextVariables str = nub $ (getVarsTextVariablesUnbracketed str) ++ (getVarsTextVariablesBracketed str)

getVarValues :: VariableMap -> [String] -> [(String, String)]
getVarValues _ [] = []
getVarValues m (v:vs) = curElement ++ (getVarValues m vs)
    where curElement = case getVariableMaybe v m of
                            Just val -> [(v, renderVariableElement val)]
                            Nothing  -> []

replaceVarInText :: (String, String) -> String -> String
replaceVarInText (s, r) t = replace ("${" ++ s ++ "}") r $ replace ("$" ++ s) r t

replaceVarsInText :: VariableMap -> String -> String
replaceVarsInText varMap str = foldr replaceVarInText str (getVarValues varMap $ getVarsTextVariables str)

------------------------
-- Render mathematical expressions in text
------------------------

renderRealResult :: Double -> String
renderRealResult x = if isInt x then show $ fromIntegral $ round x else show x

getExprLiteralsInText :: String -> [String]
getExprLiteralsInText str = map (init . drop 1) (getMatches str "`(.*?)`")

getExprValues :: VariableMap -> [String] -> [(String, String)]
getExprValues _ [] = []
getExprValues m (ex:exs) = curElement ++ (getExprValues m exs)
        where curElement = case evalRealExpr m ex of
                                Just val -> [(ex, renderRealResult val)]
                                Nothing  -> []

replaceExprInText :: (String, String) -> String -> String
replaceExprInText (s, r) t = replace ("`" ++ s ++ "`") r t

replaceExpressionsInText :: VariableMap -> String -> String
replaceExpressionsInText varMap str = foldr replaceExprInText str (getExprValues varMap $ getExprLiteralsInText str)