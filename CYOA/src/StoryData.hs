module StoryData where 

-- | Representation of a complete CYOA story
data Story = Story {
    storyTitle :: String, -- ^ The title of the story
    startPage :: Page     -- ^ The starting page (i.e. the root of the story tree)
    }  deriving (Eq)

-- | A 'page' of story, representing a node in the story tree
data Page = Page {
    pageText :: [String],       -- ^ The list of text lines comprising the page
    pageResult :: Consequence   -- ^ The consequence (an ending or question) at the end of the page
    } deriving (Eq)

-- | A choice with a label leading to a given page of story; essentially, a fork in the story tree
data Choice = Choice {
    choiceLabel :: String, -- ^ The text label associated with this choice
    nextPage :: Page       -- ^ The page this choice leads to
    } deriving (Eq)

-- | An ending for the story, representing a dead-end in the story tree
data Ending = Ending String deriving (Eq)

-- | Access the text of an ending
getEndingText :: Ending -> String
getEndingText (Ending text) = text

-- | The consequence encountered at the end of a page: either an ending or a choice leading to further pages
data Consequence = EndPoint Ending
                 | Choices {
                            choiceQuestion :: String, -- ^ The question presented
                            choiceOptions :: [Choice] -- ^ The list of possible choices
                            }
    deriving (Eq)

