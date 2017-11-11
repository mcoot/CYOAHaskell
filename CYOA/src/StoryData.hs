module StoryData where 

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

------------------------
-- Story tree structure
-------------------------

-- | Representation of a complete CYOA story
data Story = Story {
    storyTitle :: String, -- ^ The title of the story
    startPage :: Page     -- ^ The starting page (i.e. the root of the story tree)
    }  deriving (Eq)

-- | A 'page' of story, representing a node in the story tree
data Page = Page {
    pageContents :: [PageElement],       -- ^ The page data
    pageResult :: Consequence   -- ^ The consequence (an ending or question) at the end of the page
    } deriving (Eq)

-- | A choice with a label leading to a given page of story; essentially, a fork in the story tree
data Choice = Choice {
    choiceLabel :: Line, -- ^ The text label associated with this choice
    nextPage :: Page       -- ^ The page this choice leads to
    } deriving (Eq)

-- | An ending for the story, representing a dead-end in the story tree
newtype Ending = Ending {endingLine :: Line} deriving (Eq)

-- | The consequence encountered at the end of a page: either an ending or a choice leading to further pages
data Consequence = EndPoint Ending
                 | Choices {
                            choiceQuestion :: Line, -- ^ The question presented
                            choiceOptions :: [Choice] -- ^ The list of possible choices
                            }
    deriving (Eq)

------------------------
-- Lines and Variables
------------------------

type PageElement = Either Line VariablePrompt

newtype Line = Line {lineText :: String} deriving (Eq, Show)

pLine :: String -> PageElement
pLine = Left . Line

data VariablePrompt = VariablePrompt { varPromptLine :: Line, varPromptVariable :: String, varPromptType :: VariableType } deriving (Eq)

renderVarsText :: VariableMap -> String -> String
renderVarsText varMap text = text
    
-- | Type alias for a variable mapping
type VariableMap = Map String VariableElement

emptyVariableMap :: VariableMap
emptyVariableMap = Map.empty

-- | Get a variable from the map, with a default value
getVariable :: String -> VariableElement -> VariableMap -> VariableElement
getVariable k d m = Map.findWithDefault d k m

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
renderVariableElement (StringElement val) = show val
renderVariableElement (DoubleElement val) = show val
    