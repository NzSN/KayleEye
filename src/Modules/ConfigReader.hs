module Modules.ConfigReader where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language

import Data.Map as Map (Map, fromList, empty, lookup, toList)
import Data.Map.Merge.Strict
import Data.Maybe
import Data.List.Split

data Configs = Configs_M { configMap :: Map String Configs }
              | Configs_L { configList :: [Configs] }
              | Configs_Str { configVal :: [Char] }
              | Configs_Empty deriving Show

data Projects_cfg = ProjectsConfig_cfg { projName :: String } deriving Show
data TestProject_cfg = TestProject_cfg { testContent :: Map String [String] } deriving Show
data EmailInfo_cfg = EmailInfo_cfg { host :: String, user :: String, pass :: String } deriving Show
data AdminEmailAddr_cfg = AdminEmailAddr_cfg { adminEmailAddr :: String } deriving Show
data AcceptApi_cfg = AcceptApi_cfg { a_api :: String } deriving Show
data RebaseApi_cfg = RebaseApi_cfg { r_api :: String } deriving Show
data TestContent_cfg = TestContent_cfg { content :: [String] } | TestContent_None deriving Show
data DatabaseInfo_cfg = DatabaseInfo_cfg
  { db_host :: String, db_user :: String, db_pass :: String, db :: String } deriving Show
data ServerInfo_cfg = ServerInfo_cfg { addr :: String, port :: String } deriving Show

-- Configuration file parser
configFile :: GenParser Char st Configs
configFile = do
  skipMany configCommentOrBlank
  c <- sepEndBy configOption (char '\n' >> skipMany configCommentOrBlank)
  return $ foldl (\(Configs_M x) (Configs_M y) -> Configs_M $ merge1 x y) (Configs_M empty) c

  where merge1 m1 m2 = merge
                 (mapMaybeMissing (\k v -> return v))
                 (mapMaybeMissing (\k v -> return v))
                 (zipWithMaybeMatched (\k v1 v2 -> return v1))
                 m1 m2


configCommentOrBlank :: GenParser Char st ()
configCommentOrBlank = do
  (char '#' >> many (noneOf "\n")) <|> many mySpace
  eol
  return ()

configOption :: GenParser Char st Configs
configOption = do
  head <- optHead
  sep  <- optDefSeperate
  def  <- try optDefs <|> optDef
  return $ Configs_M $ fromList [(head, def)]

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

optDef :: GenParser Char st Configs
optDef = do
  def <- defStmt
  return def

optDefs :: GenParser Char st Configs
optDefs = do
  eol
  optDefPrefix
  x <- defStmt
  xs <- try optDefs <|> return (Configs_L [Configs_Empty])

  case x of
    Configs_L l -> return $ Configs_L $ (configList x) ++ (configList xs)
    Configs_M m -> return $ Configs_M $ merge1 m (configMap xs)
  where merge1 m1 m2 = merge
                       (mapMaybeMissing (\k v -> return v))
                       (mapMaybeMissing (\k v -> return v))
                       (zipWithMaybeMatched (\k v1 v2 -> return v1))
                       m1 m2

defStmt :: GenParser Char st Configs
defStmt = do
  ret <- defArray <|> defPair <|> defObj <|> defMap
  return ret

defObj :: GenParser Char st Configs
defObj = do
  str <- myString
  return $ Configs_Str str

defArray :: GenParser Char st Configs
defArray = do
  char '['
  body <- defArray_body
  char ']'
  return body

defMap :: GenParser Char st Configs
defMap = do
  char '{'
  skipMany (try mySpace)
  key <- myString
  skipMany (try mySpace)
  char ':'
  body <- defArray_body
  char '}'
  return $ Configs_M $ fromList [(key, body)]

defArray_body :: GenParser Char st Configs
defArray_body = do
  skipMany (try mySpace)

  str <- myString

  strN <- (skipMany (try mySpace) >> char ',' >> defArray_body) <|> return Configs_Empty
  return $ Configs_L $ (Configs_Str str):(configList strN)

defPair :: GenParser Char st Configs
defPair = do
  char '('
  left <- myString
  spaces
  char ','
  spaces
  right <- myString
  char ')'
  return $ Configs_L [Configs_Str left, Configs_Str right]

eol :: GenParser Char st Char
eol = try (char '\n') <|>
      fail "Error: Can't find end of line"

myString :: GenParser Char st String
myString = many (noneOf "\n,(){}[]")

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

searchConfig :: String -> Configs -> Maybe Configs
searchConfig optKey Configs_Empty = Nothing
searchConfig optKey opts = do
  case Map.lookup optKey map of
    Nothing -> Nothing
    Just c -> return c

  where map = configMap opts

listProjConfig :: Configs -> Maybe Projects_cfg
listProjConfig opts = do
  if isNothing prjs
    then Nothing
    else let prj_ = fromJust prjs
         in return $ ProjectsConfig_cfg (configVal $ head $ configList prj_)
  where prjs = searchConfig "Projects" opts

isProjExists :: String -> Configs -> Bool
isProjExists prjName opts = if isNothing prjs
                               then False
                               else elem True [ (configVal x) == prjName | x <- (configList $ fromJust $ prjs) ]
  where prjs = searchConfig "Projects" opts

configRetrive :: Configs -> String -> (Configs -> b) -> Maybe b
configRetrive opts optName f = do
  let optVals = searchConfig optName opts

  if isNothing optVals
    then Nothing
    else return $ f $ fromJust optVals

mrAcceptApiConfig :: Configs -> Maybe AcceptApi_cfg
mrAcceptApiConfig opts = configRetrive opts "MRAcceptApi"
                         (\cfg -> AcceptApi_cfg $ configVal $ head $ configList $ cfg)

mrRebaseApiConfig :: Configs -> Maybe RebaseApi_cfg
mrRebaseApiConfig opts = configRetrive opts "MRRebaseApi"
                         (\cfg -> RebaseApi_cfg $ configVal $ head $ configList cfg)

emailInfoGet :: Configs -> Maybe EmailInfo_cfg
emailInfoGet opts = configRetrive opts "Email"
  (\(Configs_L cfg_l) -> EmailInfo_cfg (configVal $ head cfg_l) (configVal $ head . tail $ cfg_l) (configVal $ last cfg_l))

adminEmailGet :: Configs -> Maybe AdminEmailAddr_cfg
adminEmailGet opts = configRetrive opts "AdminEmail" (\(Configs_L cfg) -> AdminEmailAddr_cfg $ configVal $ head cfg)

testCmdGet :: Configs -> Maybe TestContent_cfg
testCmdGet opts = configRetrive opts "TestCmd" (\(Configs_L cfg) -> TestContent_cfg $ [configVal x | x <- cfg])

testPiecesGet :: String -> Configs -> Maybe TestProject_cfg
testPiecesGet projName opts = configRetrive opts "TestProject" (getContent)
  where getContent (Configs_M m) =  TestProject_cfg $ fromList [ (x, cValsToStrs y)| (x, Configs_L y) <- toList m]
        cValsToStrs cVals = [ configVal cv | cv <- cVals]

databaseGet :: Configs -> Maybe DatabaseInfo_cfg
databaseGet opts = configRetrive opts "Database"
  (\(Configs_L cfg) -> DatabaseInfo_cfg (configVal $ head cfg)
                       (configVal $ head . tail $ cfg)
                       (configVal $ head . tail . tail $ cfg)
                       (configVal $ last cfg))

serverInfoGet :: Configs -> Maybe ServerInfo_cfg
serverInfoGet opts = configRetrive opts "ServerAddr"
  (\(Configs_L cfg) -> ServerInfo_cfg (configVal $ head cfg) (configVal $ last cfg))
