module VariableData (VariableType(IntVariable, StringVariable, DoubleVariable), 
                     VariableElement(IntElement, StringElement, DoubleElement), 
                     VariableMap,
                     getVariable, getVariableMaybe,
                     putVariable,
                     emptyVariableMap,
                     renderVarsText) where

import Data.List
import Text.Regex.Posix
import Data.String.Utils
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

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
getVarValues m (v:vs)
    = curElement ++ (getVarValues m vs)
    where curElement = case getVariableMaybe v m of
                            Just val -> [(v, renderVariableElement val)]
                            Nothing -> []

replaceVarInText :: (String, String) -> String -> String
replaceVarInText (s, r) t = replace ("${" ++ s ++ "}") r $ replace ("$" ++ s) r t

renderVarsText :: VariableMap -> String -> String
renderVarsText varMap text = foldr replaceVarInText text (getVarValues varMap $ getVarsTextVariables text)
    
-- | Type alias for a variable mapping
type VariableMap = Map String VariableElement

emptyVariableMap :: VariableMap
emptyVariableMap = Map.empty

-- | Get a variable from the map, with a default value
getVariable :: String -> VariableElement -> VariableMap -> VariableElement
getVariable k d m = Map.findWithDefault d k m

getVariableMaybe :: String -> VariableMap -> Maybe VariableElement
getVariableMaybe = Map.lookup

-- | Put a variable in the map
putVariable :: String -> VariableElement -> StateT VariableMap IO ()
putVariable k v = do
    m <- get
    put $ Map.insert k v m

mapTest :: StateT VariableMap IO ()
mapTest = do
    lift $ putStrLn "Printing shit"
    putVariable "Memes" (StringElement "a bird")
    lift $ putStrLn "And another thing..."
    m <- get
    -- let potato = getVariable "Memes" (StringElement "a frog") m
    thing <- return $ getVariable "Memes" (StringElement "a frog") m
    lift $ putStrLn $ "I retrieved " ++ (show thing)
    return ()
    

-- | The possible types of variables that may be stored in the map
data VariableType = IntVariable
                  | StringVariable
                  | DoubleVariable
    deriving (Eq, Show)

-- | Data type for the value of the variable map, which may be an integer, string or floating point
data VariableElement = IntElement Integer
                     | StringElement String
                     | DoubleElement Double
    deriving (Eq, Show)

renderVariableElement :: VariableElement -> String
renderVariableElement (IntElement val) = show val
renderVariableElement (StringElement val) = val
renderVariableElement (DoubleElement val) = show val
    