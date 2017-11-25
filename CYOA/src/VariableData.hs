module VariableData (VariableType(IntVariable, StringVariable, DoubleVariable, BoolVariable), 
                     VariableElement(IntElement, StringElement, DoubleElement, BoolElement), 
                     VariableMap,
                     getVariable, getVariableMaybe,
                     putVariable,
                     emptyVariableMap) where

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
    
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
                  | BoolVariable
    deriving (Eq, Show)

-- | Data type for the value of the variable map, which may be an integer, string or floating point
data VariableElement = IntElement Integer
                     | StringElement String
                     | DoubleElement Double
                     | BoolElement Bool
    deriving (Eq, Show)
    