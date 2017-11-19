module StoryData where 

import VariableData
import VariableExpressions

------------------------
-- Story tree structure
------------------------

-- | Representation of a complete CYOA story
data Story = Story {
    storyTitle :: String, -- ^ The title of the story
    startPage :: Page     -- ^ The starting page (i.e. the root of the story tree)
    }

-- | A 'page' of story, representing a node in the story tree
data Page = Page {
    pageContents :: [PageElement],       -- ^ The page data
    pageResult :: Consequence   -- ^ The consequence (an ending or question) at the end of the page
    }

-- | A choice with a label leading to a given page of story; essentially, a fork in the story tree
data Choice = Choice {
    choiceLabel :: Line, -- ^ The text label associated with this choice
    nextPage :: Page       -- ^ The page this choice leads to
    }

-- | An ending for the story, representing a dead-end in the story tree
newtype Ending = Ending {endingLine :: Line} deriving (Eq)

-- | A branch in the story tree made based on a predicate, without user interaction
data Branch = Branch {
    branchPred :: String,
    branchTruePage :: Page,
    branchFalsePage :: Page
    }

-- | The consequence encountered at the end of a page: either an ending or a choice leading to further pages
data Consequence = EndPoint Ending
                 | Choices {
                            choiceQuestion :: Line, -- ^ The question presented
                            choiceOptions :: [Choice] -- ^ The list of possible choices
                           }
                 | Conditional {
                                condBranch :: Branch,
                                condTrueLine :: Line,
                                condFalseLine :: Line
                               }

------------------------
-- Lines and Variables
------------------------

type PageElement = Either Line VariablePrompt

newtype Line = Line {lineText :: String} deriving (Eq, Show)

pLine :: String -> PageElement
pLine = Left . Line

data VariablePrompt = VariablePrompt { varPromptLine :: Line, varPromptVariable :: String, varPromptType :: VariableType }

pVarPrompt :: String -> String -> VariableType -> PageElement
pVarPrompt dispLine varName varType = Right $ VariablePrompt (Line dispLine) varName varType