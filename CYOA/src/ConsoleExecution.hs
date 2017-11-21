module ConsoleExecution (playStory) where

import Control.Monad.State
import System.IO
import qualified Text.Read (readMaybe)

import VariableData
import VariableExpressions
import LineRendering
import StoryData
import Lib

------------------------
-- Basic IO with StateT context
------------------------

-- | Print a string to the console
sPrintLn :: String -> StateT VariableMap IO ()
sPrintLn = lift . putStrLn

-- | Get a string line from the user
sGetLn :: StateT VariableMap IO String
sGetLn = lift $ getLine

-- | Wait for an enter press, not echoing and ignoring stdin
sGetEnterPress :: StateT VariableMap IO ()
sGetEnterPress = do
    lift $ hSetEcho stdin False
    _ <- lift $ getLine
    lift $ hSetEcho stdin True

------------------------
-- Story Execution
------------------------

-- | Pretty-print a string to the screen as a title
printTitle :: String -> IO ()
printTitle title = do
    putStrLn $ duplicate "-" $ length title + 4
    putStrLn $ "| " ++ title ++ " |"
    putStrLn $ duplicate "-" $ length title + 4

displayTextLine :: Line -> StateT VariableMap IO ()
displayTextLine (Line text) = do
    m <- get
    sPrintLn $ renderVarsText m text
    sGetEnterPress

promptForVariable :: VariableType -> String -> StateT VariableMap IO ()
promptForVariable varType varName = do
    response <- sGetLn
    case varType of
        StringVariable  -> putVariable varName (StringElement response)
        IntVariable     -> case Text.Read.readMaybe response of
                                Just x   -> putVariable varName (IntElement x)
                                Nothing  -> promptForVariable varType varName
        DoubleVariable  -> case Text.Read.readMaybe response of
                                Just x   -> putVariable varName (DoubleElement x)
                                Nothing  -> promptForVariable varType varName

displayVariablePrompt :: VariablePrompt -> StateT VariableMap IO ()
displayVariablePrompt (VariablePrompt {varPromptLine = line, 
                                       varPromptVariable = varName,
                                       varPromptType = varType})
    = do
        m <- get
        sPrintLn $ renderVarsText m (lineText line)
        promptForVariable varType varName

performVariableAssignment :: (VariableMap -> String -> Maybe a) -> (a -> VariableElement) -> VariableAssignment -> StateT VariableMap IO ()
performVariableAssignment evalFn constructorFn va = do
    m <- get
    let realRes = evalFn m (varAssignValueExpr va)
    case realRes of
        Just r  -> putVariable (varAssignVariable va) (constructorFn r)
        Nothing -> sPrintLn $ "[Evaluation of `" ++ varAssignValueExpr va ++ "` in variable assignment failed]"  

executeVariableAssignment :: VariableAssignment -> StateT VariableMap IO ()
executeVariableAssignment va = case varAssignType va of
                                    IntVariable     -> performVariableAssignment evalRealExpr (IntElement . floor) va
                                    DoubleVariable  -> performVariableAssignment evalRealExpr DoubleElement va 
                                    StringVariable  -> performVariableAssignment evalStringExpr StringElement va


-- | Display the elements of a page one line at a time
displayPageContents :: [PageElement] -> StateT VariableMap IO ()
displayPageContents [] = return ()
displayPageContents (x:xs) = do
    case x of
        PELine line     -> displayTextLine line
        PEPrompt prompt -> displayVariablePrompt prompt
        PEAssign a      -> executeVariableAssignment a
    displayPageContents xs

-- | Helper function for printing a list of choices
printChoicesLoop :: [Choice] -> Integer -> StateT VariableMap IO ()
printChoicesLoop [] _ = return ()
printChoicesLoop (x:xs) i = do
    m <- get
    sPrintLn $ (show i) ++ ") " ++  (renderVarsText m $ lineText $ choiceLabel x)
    printChoicesLoop xs (i + 1)

-- | Print a list of choices
printChoices :: [Choice] -> StateT VariableMap IO ()
printChoices choices = printChoicesLoop choices 1

-- | Wait for a valid input of a number from 1 to the number of choices, 
--   and return the associated choice
getUserChoice :: [Choice] -> IO Choice
getUserChoice choices = do
    putStr "> "
    hFlush stdout
    response <- getLine
    let parseResult = reads response in
        if length parseResult == 0 then
            getUserChoice choices
        else
            let index = fst (parseResult !! 0) in
                return $ choices !! (index - 1)

-- | Present a question with a list of possible responses, and get a response input
presentChoice :: Line -> [Choice] -> StateT VariableMap IO Choice
presentChoice question choices = do
    m <- get
    sPrintLn $ renderVarsText m $ lineText question
    printChoices choices
    -- Get an answer
    lift $ getUserChoice choices

-- | Get the page to move to from a branch condition; defaults to false if predicate evaluation fails
presentBranch :: Branch -> Line -> Line -> StateT VariableMap IO Page
presentBranch br tl fl = do
    m <- get
    case evalPredExpr m (branchPred br) of
        Just b -> if b then
                    (displayTextLine tl) >> (return $ branchTruePage br )
                  else
                    (displayTextLine fl) >> (return $ branchFalsePage br)
        Nothing -> (displayTextLine fl) >> (return $ branchFalsePage br)

-- | Execute a story page, printing the story line-by-line
--   If the page ends in a Choice, the choice is prompted and the page the user's choice leads to is returned
--   If the page ends with an Ending, this is returned
runPage :: Page -> StateT VariableMap IO (Either Page Ending)
runPage page = do
    displayPageContents $ pageContents page
    case pageResult page of
        EndPoint ending -> do
            return $ Right ending
        Choices {choiceQuestion = question, choiceOptions = options} -> do
            userChoice <- presentChoice question options
            return $ Left $ nextPage userChoice
        Conditional {condBranch = br, condTrueLine = tl, condFalseLine = fl} -> do
            condPage <- presentBranch br tl fl
            return $ Left $ condPage

-- | Run a page, and then the follow on page, until eventually an ending is reached
--   i.e. traverse the story tree with user input guiding the choices
runPageTraversal :: Page -> StateT VariableMap IO Ending
runPageTraversal page = do
    nextResult <- runPage page
    case nextResult of
        Right ending -> return ending
        Left nextPage -> runPageTraversal nextPage

-- | Execute a story via console interaction with the user
playStory :: Story -> IO ()
playStory story = do
    printTitle $ storyTitle story
    result <- runStateT (runPageTraversal $ startPage story) emptyVariableMap
    putStrLn $ lineText $ endingLine $ fst result
    putStrLn "[END]"

-- playStory = undefined