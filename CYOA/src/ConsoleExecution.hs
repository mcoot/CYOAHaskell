module ConsoleExecution (playStory) where

import System.IO

import StoryData
import Lib

-- | Pretty-print a string to the screen as a title
printTitle :: String -> IO ()
printTitle title = do
    putStrLn $ duplicate "-" $ length title + 4
    putStrLn $ "| " ++ title ++ " |"
    putStrLn $ duplicate "-" $ length title + 4

-- | Display text one line at a time, waiting for an Enter press between lines
displayText :: [String] -> IO ()
displayText [] = return ()
displayText (x:xs) = do
    putStrLn x
    -- Turn echo off so that text entered isn't shown
    -- and also so there isn't a blank line between each item
    hSetEcho stdin False
    _ <- getLine
    hSetEcho stdin True
    displayText xs

-- | Helper function for printing a list of choices
printChoicesLoop :: [Choice] -> Integer -> IO ()
printChoicesLoop [] _ = return ()
printChoicesLoop (x:xs) i = do
    putStrLn $ (show i) ++ ") " ++ choiceLabel x
    printChoicesLoop xs (i + 1)

-- | Print a list of choices
printChoices :: [Choice] -> IO ()
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
presentChoice :: String -> [Choice] -> IO Choice
presentChoice question choices = do
    putStrLn question
    printChoices choices
    -- Get an answer
    getUserChoice choices

-- | Execute a story page, printing the story line-by-line
--   If the page ends in a Choice, the choice is prompted and the page the user's choice leads to is returned
--   If the page ends with an Ending, this is returned
runPage :: Page -> IO (Either Page Ending)
runPage page = do
    displayText $ pageText page
    case pageResult page of
        EndPoint ending -> do
            return $ Right ending
        Choices {choiceQuestion = question, choiceOptions = options} -> do
            userChoice <- presentChoice question options
            return $ Left $ nextPage userChoice

-- | Run a page, and then the follow on page, until eventually an ending is reached
--   i.e. traverse the story tree with user input guiding the choices
runPageTraversal :: Page -> IO Ending
runPageTraversal page = do
    nextResult <- runPage page
    case nextResult of
        Right ending -> return ending
        Left nextPage -> runPageTraversal nextPage

-- | Execute a story via console interaction with the user
playStory :: Story -> IO ()
playStory story = do
    printTitle $ storyTitle story
    ending <- runPageTraversal $ startPage story
    putStrLn $ getEndingText ending
    putStrLn "[END]"