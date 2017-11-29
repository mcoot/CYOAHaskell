module StoryFiles (loadStory, saveStory) where

import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)

import StoryData

loadStory :: String -> IO Story
loadStory = undefined

saveStory :: Story -> String -> IO ()
saveStory = undefined

------------------------
-- File Loading
------------------------

-- | Need to parse in two stages, because pages early in the file may reference those that come later
data FirstPassResult = FirstPassResult {firstPassTitle :: String, firstPassPages :: [FirstPassPage]} deriving (Show)

data FirstPassPage = FirstPassPage {firstPassPageTitle :: String, firstPassPageContent :: String} deriving (Show)

getFilePages :: FilePath -> IO (Either ParseError FirstPassResult)
getFilePages pth = parseFromFile parseFileFirstPass pth

parseDecl :: String -> Parser String
parseDecl declName = between (string $ "[" ++ declName ++ " ") (char ']') (many (alphaNum <|> char '_' <|> space))

parseFilePage :: Parser FirstPassPage
parseFilePage = do
    pname <- parseDecl "PAGE"
    pcontent <- manyTill anyChar (lookAhead $ (() <$ (try $ parseDecl "PAGE")) <|> eof)
    return $ FirstPassPage pname pcontent


parseFilePages :: Parser [FirstPassPage]
parseFilePages = many parseFilePage

parseFileFirstPass :: Parser FirstPassResult
parseFileFirstPass = do
    skipMany newline
    title <- parseDecl "TITLE"
    skipMany newline
    pages <- parseFilePages
    return $ FirstPassResult title pages



------------------------
-- File Saving
------------------------