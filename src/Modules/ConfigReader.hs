module Modules.ConfigReader where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Data.Maybe
import Data.List.Split

type Configs = [[String]]
type Config = [String]

data Projects_cfg = ProjectsConfig_cfg { projName :: String } deriving Show
data TestProject_cfg = TestProject_cfg { testName :: String, testContent :: [String]  } deriving Show
data EmailInfo_cfg = EmailInfo_cfg { host :: String, user :: String, pass :: String } deriving Show
data AdminEmailAddr_cfg = AdminEmailAddr_cfg { adminEmailAddr :: String } deriving Show
data AcceptApi_cfg = AcceptApi_cfg { a_api :: String } deriving Show
data RebaseApi_cfg = RebaseApi_cfg { r_api :: String } deriving Show
data TestContent_cfg = TestContent_cfg { content :: [String] } deriving Show
data DatabaseInfo_cfg = DatabaseInfo_cfg
  { db_host :: String, db_user :: String, db_pass :: String, db :: String } deriving Show
data ServerInfo_cfg = ServerInfo_cfg { addr :: String, port :: String } deriving Show

-- Configuration file parser
configFile :: GenParser Char st Configs
configFile = do
  skipMany configCommentOrBlank
  sepEndBy configOption (char '\n' >> skipMany configCommentOrBlank)

configCommentOrBlank :: GenParser Char st Config
configCommentOrBlank = do
  (char '#' >> many (noneOf "\n")) <|> many mySpace
  eol
  return []

configOption :: GenParser Char st Config
configOption = do
  head <- optHead
  sep  <- optDefSeperate
  def  <- try optDefs <|> optDef
  return (head:def)

optHead :: GenParser Char st String
optHead = do
  def <- try (string "TestCmd")       <|>
         try (string "MRAccessApi")   <|>
         try (string "Projects")      <|>
         try (string "MRAcceptApi")   <|>
         try (string "MRRebaseApi")   <|>
         try (string "Email")         <|>
         try (string "TestProject")   <|>
         try (string "Database")      <|>
         try (string "ServerAddr")    <|>
         (string "AdminEmail")
  return def

optDefSeperate :: GenParser Char st String
optDefSeperate = do
  skipMany (try mySpace)
  string ":"
  skipMany (try mySpace)
  return ":"

optDef :: GenParser Char st Config
optDef = do
  def <- defStmt
  return def

optDefs :: GenParser Char st Config
optDefs = do
  eol
  optDefPrefix
  x <- defStmt
  xs <- try optDefs <|> return []
  return (x ++ xs)

defStmt :: GenParser Char st Config
defStmt = do
  ret <- defArray <|> defPair <|> defObj
  return ret

defObj :: GenParser Char st Config
defObj = do
  str <- myString
  return (str:[])

defArray :: GenParser Char st Config
defArray = do
  char '['
  body <- defArray_body
  char ']'
  return body

defArray_body :: GenParser Char st Config
defArray_body = do
  skipMany (try mySpace)

  str <- myString

  strN <- (skipMany (try mySpace) >> char ',' >> defArray_body) <|> return []
  return $ str:strN

defPair :: GenParser Char st Config
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

parseConfig :: String -> Either ParseError Configs
parseConfig input = parse configFile "(unknown)" input

searchConfig :: String -> Configs -> Maybe Config
searchConfig optKey [] = Nothing
searchConfig optKey opts = do
  let result = filter (\(x:xs) -> x == optKey) $ opts
  if null result
    then Nothing
    else return $ tail . head $ result

listProjConfig :: Configs -> Maybe Config
listProjConfig opts = do
  if prjs == Nothing
    then Nothing
    else return $ fromJust $ prjs
  where prjs = searchConfig "Projects" opts

isProjExists :: String -> Configs -> Bool
isProjExists prjName opts = if prjs == Nothing
                               then False
                               else elem True [ x == prjName | x <- (fromJust $ prjs) ]
  where prjs = searchConfig "Projects" opts

windRiverPathConfig :: Configs -> Maybe String
windRiverPathConfig [] = Nothing
windRiverPathConfig opts = do
  path <- searchConfig "WindRiverPath" opts
  return $ head path

tempDirPathConfig :: Configs -> Maybe String
tempDirPathConfig [] = Nothing
tempDirPathConfig opts = do
  path <- searchConfig "TempDirPath" opts
  return $ head path

pairValueSearch :: String -> String -> Configs -> Maybe String
pairValueSearch prjName opt opts = do
  apis <- let mr = searchConfig opt opts
          in if mr == Nothing
                then Just []
                else mr
  if apis == []
     then Nothing
     else return $ last $ head [ splitOn " " x | x <- apis, isTheApi x ]
  where isTheApi = (== prjName) . head . splitOn " "

mrAccessApiConfig :: String -> Configs -> Maybe String
mrAccessApiConfig prjName opts = pairValueSearch prjName "MRAccessApi" opts

sourceUrlConfig :: String -> Configs -> Maybe String
sourceUrlConfig prjName opts = pairValueSearch prjName "SourceUrl" opts

configRetrive :: Configs -> String -> (Config -> b) -> Maybe b
configRetrive opts optName f = do
  let optVals = searchConfig optName opts

  if isNothing optVals
    then Nothing
    else return $ f $ fromJust optVals

mrAcceptApiConfig :: Configs -> Maybe AcceptApi_cfg
mrAcceptApiConfig opts = configRetrive opts "MRAcceptApi" (\cfg -> AcceptApi_cfg $ head cfg)

mrRebaseApiConfig :: Configs -> Maybe RebaseApi_cfg
mrRebaseApiConfig opts = configRetrive opts "MRRebaseApi" (\cfg -> RebaseApi_cfg $ head cfg)

emailInfoGet :: Configs -> Maybe EmailInfo_cfg
emailInfoGet opts = configRetrive opts "Email"
  (\cfg -> EmailInfo_cfg (head cfg) (head . tail $ cfg) (last cfg))

adminEmailGet :: Configs -> Maybe AdminEmailAddr_cfg
adminEmailGet opts = configRetrive opts "AdminEmail" (\cfg -> AdminEmailAddr_cfg $ head cfg)

testCmdGet :: Configs -> Maybe TestContent_cfg
testCmdGet opts = configRetrive opts "TestCmd" (\cfg -> TestContent_cfg cfg)

testPiecesGet :: Configs -> Maybe TestProject_cfg
testPiecesGet opts = configRetrive opts "TestProject" (\cfg -> TestProject_cfg (head cfg) (tail cfg))

databaseGet :: Configs -> Maybe DatabaseInfo_cfg
databaseGet opts = configRetrive opts "Database"
  (\cfg -> DatabaseInfo_cfg (head cfg) (head . tail $ cfg) (head . tail . tail $ cfg) (last cfg))

serverInfoGet :: Configs -> Maybe ServerInfo_cfg
serverInfoGet opts = configRetrive opts "ServerAddr"
  (\cfg -> ServerInfo_cfg (head cfg) (last cfg))
