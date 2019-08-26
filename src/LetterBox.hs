-- file: LetterBox.hs

module LetterBox where

import Data.Aeson
import Data.Map as Map
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Database.HDBC
import Database.HDBC.MySQL

import Data.Maybe
import Data.ByteString.Lazy.Internal

import Homer

-- Unit Testing
import Test.HUnit

procTbl :: String
procTbl = "processing"

historyTbl :: String
historyTbl = "history"

-- Prepared Statements
databaseCreateStmt :: String
databaseCreateStmt = "CREATE DATABASE kayleHome"

tableCreateStmt :: String -- Table name
                -> String
tableCreateStmt tblN = "CREATE TABLE " ++ tblN ++ " (" ++
                       "ident VARCHAR(50)," ++
                       "content VARCHAR(255)," ++
                       "PRIMARY KEY(ident))"

contentUpdateStmt :: String
contentUpdateStmt = "UPDATE " ++ procTbl ++
                   " SET content = ?" ++
                   " WHERE ident = ?"

searchLetterStmt :: String -> String
searchLetterStmt tblN = "SELECT ident,content FROM " ++ tblN ++ " WHERE ident = ?"

data BoxKey = BoxKey { key :: Connection }

boxInit :: BoxKey -> IO ()
boxInit key_ = do
  let conn = key key_

  tables <- quickQuery' conn ("SHOW TABLES") []
  let tblN = Prelude.map (\[sqlTblN] -> fromSql sqlTblN) tables :: [String]

  if elem procTbl tblN
    then return ()
    else run conn (tableCreateStmt procTbl) [] >> return ()
  if elem historyTbl tblN
    then return ()
    else run conn (tableCreateStmt historyTbl) [] >> return ()

  commit conn

boxKeyCreate :: String -- Host name
             -> String -- User name
             -> String -- Password
             -> String -- Database name
             -> IO BoxKey
boxKeyCreate hn un pn dn = do
  conn <- connectMySQL defaultMySQLConnectInfo {
    mysqlHost = hn,
    mysqlUser = un,
    mysqlPassword = pn,
    mysqlDatabase = dn }
  return $ BoxKey conn

-- Search letter from database via proj name and sha-1 value
insertLetter :: BoxKey
             -> String -- Table name
             -> Letter
             -> IO ()
insertLetter key_ tblN letter = do
  run (key key_) ("INSERT INTO " ++ tblN ++ " VALUES (?, ?)")
    [toSql (ident letter), toSql (encode $ (content letter))]
  commit (key key_)

searchLetter :: BoxKey
             -> String -- Table name
             -> String -- Identity
             -> MaybeT IO Letter
searchLetter key_ tbl ident_ = MaybeT $ do
  letters <- quickQuery' (key key_) (searchLetterStmt tbl) [toSql ident_]
  if not $ Prelude.null letters
    then return $ Just $ head $ Prelude.map toLetter letters
    else return Nothing

  where toLetter :: [SqlValue] -> Letter
        toLetter [sqlIdent, sqlContent] =
          Letter (fromSql sqlIdent) Map.empty
          -- fixme: decode may return Nothing
          (fromJust $ (decode $ fromSql sqlContent :: Maybe (Map String String)))

removeLetter :: BoxKey
             -> String -- Table name
             -> String -- Identity
             -> IO ()
removeLetter key_ tbl ident_ = do
  run (key key_) ("DELETE FROM " ++ tbl ++ " WHERE ident = ?") [toSql ident_]
  commit (key key_)

isLetterExists :: BoxKey
               -> String -- Table name
               -> String -- Identity
               -> IO Bool
isLetterExists key_ tbl ident_ = do
  letter <- runMaybeT $ searchLetter key_ tbl ident_
  if Prelude.null letter
    then return False
    else return True

updateLetter :: BoxKey
             -> String     -- Identity
             -> ByteString -- Content
             -> Bool       -- Is test done?
             -> IO Int
updateLetter key_ ident_ content status = do
  letter <- runMaybeT $ searchLetter key_ procTbl ident_

  if isNothing letter
    then return 2
    else updating (fromJust letter) status

  where
    -- Decoded letter content
    decodedContent = decode content :: Maybe (Map String String)
    -- Move content from procTbl to historyTbl
    moveToHistory _ Nothing = return 3
    moveToHistory letter_ (Just x) =
      (insertLetter key_ historyTbl (Letter (ident letter_) (header letter_) x))
      >> removeLetter key_ procTbl (ident letter_)
      >> commit (key key_) >> return 1
    -- Steps to update procTbl or historyTbl
    updating l True = moveToHistory l decodedContent
    updating l False = run (key key_) contentUpdateStmt [toSql content, toSql (ident l)]
                       >> commit (key key_) >> return 0

-- Test cases
boxTest :: Test
boxTest = TestList [TestLabel "Box Insert,Search,Delete: " (TestCase boxAssert),
                    TestLabel "Box Update: " (TestCase boxAssert_Update),
                    TestLabel "Box IsExists" (TestCase boxAssert_isExists)]
  where boxAssert :: Assertion
        boxAssert = do
          key <- boxKeyCreate "127.0.0.1" "aydenlin" "ready" "try"
          -- Init test db
          boxInit key

          -- Insert, Search, Delete
          insertLetter key procTbl l
          l_r <- runMaybeT $ searchLetter key procTbl (ident l)
          removeLetter key procTbl (ident l)

          assertEqual "Insert, Search, Delete:" False (isNothing l_r)

        boxAssert_Update :: Assertion
        boxAssert_Update = do
          key <- boxKeyCreate "127.0.0.1" "aydenlin" "ready" "try"
          -- Init test db
          boxInit key

          -- Update
          let n_content = encode $ fromList [("N_T1", "N_T")]

          insertLetter key procTbl l
          updateLetter key (ident l) n_content False
          l_r_u <- runMaybeT $ searchLetter key procTbl (ident l)
          removeLetter key procTbl (ident l)

          let u_content = content $ fromJust $ l_r_u
          assertEqual "Update" False (isNothing $ Map.lookup "N_T1" u_content)

        boxAssert_isExists :: Assertion
        boxAssert_isExists = do
          key <- boxKeyCreate "127.0.0.1" "aydenlin" "ready" "try"
          -- Init test db
          boxInit key

          insertLetter key procTbl l
          isExists <- isLetterExists key procTbl (ident l)
          removeLetter key procTbl (ident l)

          assertEqual "IsExists" True isExists

        l = Letter (ident2Str $ Identity "item1" "12345")
            (fromList [("iid", "1")])
            (fromList [("T1", "T")])
