module Modules.ConfigReader (
  parseConfig,
  searchConfig,
  isProjExists,
  windRiverPathConfig,
  tempDirPathConfig,
  mrAccessApiConfig,
  sourceUrlConfig,
  listProjConfig
) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Data.Maybe
import Data.List.Split

-- Configuration file parser
configFile :: GenParser Char st [[String]]
configFile = do
  skipMany configCommentOrBlank
  sepEndBy configOption (char '\n' >> skipMany configCommentOrBlank)

configCommentOrBlank :: GenParser Char st [String]
configCommentOrBlank = do
  (char '#' >> many (noneOf "\n")) <|> many mySpace
  eol
  return []

configOption :: GenParser Char st [String]
configOption = do
  head <- optHead
  sep  <- optDefSeperate
  def  <- try optDefs <|> optDef
  return (head:def)

optHead :: GenParser Char st String
optHead = do
  def <- try (string "TempDirPath")   <|>
         try (string "MRAccessApi")   <|>
         try (string "Projects")      <|>
         try (string "SourceUrl")     <|>
         try (string "MRAcceptApi")
  return def

optDefSeperate :: GenParser Char st String
optDefSeperate = do
  skipMany (try mySpace)
  string ":"
  skipMany (try mySpace)
  return ":"

optDef :: GenParser Char st [String]
optDef = do
  def <- defStmt
  return def

optDefs :: GenParser Char st [String]
optDefs = do
  eol
  optDefPrefix
  x <- defStmt
  xs <- try optDefs <|> return []
  return (x ++ xs)

defStmt :: GenParser Char st [String]
defStmt = do
  ret <- defPair <|> defObj
  return ret

defObj :: GenParser Char st [String]
defObj = do
  str <- myString
  return (str:[])

defPair :: GenParser Char st [String]
defPair = do
  char '('
  left <- myString
  spaces
  char ','
  spaces
  right <- myString
  char ')'
  return ((left ++ " " ++ right):[])

eol :: GenParser Char st Char
eol = try (char '\n') <|>
      fail "Error: Can't find end of line"

myString :: GenParser Char st String
myString = many (noneOf " \n,(){}[]")

mySpace :: GenParser Char st Char
mySpace = do
  x <- char ' ' <|> char '\t'
  return x

optDefPrefix :: GenParser Char st String
optDefPrefix = do
  spaces
  string "-"
  spaces
  return "- "

parseConfig :: String -> Either ParseError [[String]]
parseConfig input = parse configFile "(unknown)" input

searchConfig :: String -> [[String]] -> Maybe [String]
searchConfig optKey [] = Nothing
searchConfig optKey opts = do
  let result = filter (\(x:xs) -> x == optKey) $ opts
  if null result
    then Nothing
    else return $ tail . head $ result

listProjConfig :: [[String]] -> Maybe [String]
listProjConfig opts = do
  if prjs == Nothing
    then Nothing
    else return $ fromJust $ prjs
  where prjs = searchConfig "Projects" opts

isProjExists :: String -> [[String]] -> Bool
isProjExists prjName opts = if prjs == Nothing
                               then False
                               else elem True [ x == prjName | x <- (fromJust $ prjs) ]
  where prjs = searchConfig "Projects" opts

windRiverPathConfig :: [[String]] -> Maybe String
windRiverPathConfig [] = Nothing
windRiverPathConfig opts = do
  path <- searchConfig "WindRiverPath" opts
  return $ head path

tempDirPathConfig :: [[String]] -> Maybe String
tempDirPathConfig [] = Nothing
tempDirPathConfig opts = do
  path <- searchConfig "TempDirPath" opts
  return $ head path

pairValueSearch :: String -> String -> [[String]] -> Maybe String
pairValueSearch prjName opt opts = do
  apis <- let mr = searchConfig opt opts
          in if mr == Nothing
                then Just []
                else mr
  if apis == []
     then Nothing
     else return $ last $ head [ splitOn " " x | x <- apis, isTheApi x ]
  where isTheApi = (== prjName) . head . splitOn " "

mrAccessApiConfig :: String -> [[String]] -> Maybe String
mrAccessApiConfig prjName opts = pairValueSearch prjName "MRAccessApi" opts

sourceUrlConfig :: String -> [[String]] -> Maybe String
sourceUrlConfig prjName opts = pairValueSearch prjName "SourceUrl" opts

mrAcceptApiConfig :: String -> [[String]] -> Maybe String
mrAcceptApiConfig prjName opts = pairValueSearch prjName "MRAcceptApi" opts
