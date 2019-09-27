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

memoTbl :: String
memoTbl = "memo"

-- Prepared Statements
databaseCreateStmt :: String
databaseCreateStmt = "CREATE DATABASE kayleHome"

tableCreateStmt :: String -- Table name
                -> String
tableCreateStmt tblN = "CREATE TABLE IF NOT EXISTS " ++ tblN ++ " (" ++
                       "ident VARCHAR(50)," ++
                       "content VARCHAR(255)," ++
                       "PRIMARY KEY(ident))"

-- Memory table used to keepp track of processing Letter
-- which already in history table.
memoTblCreateStmt :: String
memoTblCreateStmt = "CREATE TABLE memo (" ++
                    "ident VARCHAR(50)," ++
                    "counter INT" ++
                    ") ENGINE=MEMORY"

memoTblNewStmt :: String
memoTblNewStmt = "INSERT INTO memo VALUES(?, ?)"


memoTblSetStmt :: String
memoTblSetStmt = "UPDATE memo SET counter = ? WHERE ident = ?"

memoTblGetStmt :: String
memoTblGetStmt = "SELECT counter FROM memo WHERE ident = ?"

memoTblDescStmt :: String
memoTblDescStmt = "UPDATE memo SET counter = (counter - 1) WHERE ident = ?"

memoTblDelStmt :: String
memoTblDelStmt = "DELETE FROM memo WHERE ident = ?"

contentUpdateStmt :: String
contentUpdateStmt = "UPDATE " ++ procTbl ++
                   " SET content = ?" ++
                   " WHERE ident = ?"

searchLetterStmt :: String -> String
searchLetterStmt tblN = "SELECT ident,content FROM " ++ tblN ++ " WHERE ident = ?"

data BoxKey = BoxKey { key :: Connection } | BoxBrokenKey

boxInit :: BoxKey -> IO ()
boxInit key_ = do
  let conn = key key_

  -- Create processing table
  withRTSSignalsBlocked $ run conn (tableCreateStmt procTbl) [] >> return ()
  -- Create history table
  withRTSSignalsBlocked $ run conn (tableCreateStmt historyTbl) [] >> return ()
  -- Memory table
  withRTSSignalsBlocked $ run conn memoTblCreateStmt [] >> return ()

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

commitKey :: BoxKey -> IO ()
commitKey bKey = commit (key bKey)

-- Search letter from database via proj name and sha-1 value
insertLetter :: BoxKey
             -> String -- Table name
             -> Letter
             -> IO ()
insertLetter key_ tblN letter = do
  withRTSSignalsBlocked $ run (key key_) ("INSERT INTO " ++ tblN ++ " VALUES (?, ?)")
    [toSql (ident letter), toSql (encode $ (content letter))]
  return ()

searchLetter :: BoxKey
             -> String -- Table name
             -> String -- Identity
             -> MaybeT IO Letter
searchLetter key_ tbl ident_ = MaybeT $ do
  letters <- withRTSSignalsBlocked $ quickQuery' (key key_) (searchLetterStmt tbl) [toSql ident_]

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
  withRTSSignalsBlocked $ run (key key_) ("DELETE FROM " ++ tbl ++ " WHERE ident = ?") [toSql ident_]
  return ()

isLetterExists :: BoxKey
               -> String -- Table name
               -> String -- Identity
               -> IO Bool
isLetterExists key_ tbl ident_ = do
  letter <- runMaybeT $ searchLetter key_ tbl ident_
  if isNothing letter
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
    moveToHistory _ Nothing = return 3
    -- Move content from procTbl to historyTbl
    moveToHistory letter_ (Just x) =
      (insertLetter key_ historyTbl (Letter (ident letter_) (header letter_) x))
      >> removeLetter key_ procTbl (ident letter_) >> return 1
    -- Steps to update procTbl
    updating l True = moveToHistory l decodedContent
    updating l False = withRTSSignalsBlocked $ run (key key_) contentUpdateStmt [toSql content, toSql (ident l)]
                       >> return 0

-- Functions of memo table
insertMemo :: BoxKey -> String -> Int -> IO ()
insertMemo bKey ident c = do
  withRTSSignalsBlocked $ run (key bKey) memoTblNewStmt [toSql ident, toSql c]
  return ()

setMemoCount :: BoxKey
             -> String -- Ident of letter
             -> Int    -- Counter's value
             -> IO ()
setMemoCount bKey ident count = do
  withRTSSignalsBlocked $ run (key bKey) memoTblSetStmt [toSql ident, toSql count]
  return ()

getMemoCount :: BoxKey
             -> String
             -> IO Int
getMemoCount bKey ident = do
  count <- withRTSSignalsBlocked $ quickQuery' (key bKey) memoTblGetStmt [toSql ident]
  return $ head $ Prelude.map toCount count
  where
    toCount :: [SqlValue] -> Int
    toCount [sqlCount] = fromSql sqlCount

isMemoExists :: BoxKey -> String -> IO Bool
isMemoExists bKey ident = do
  count <- withRTSSignalsBlocked $ quickQuery' (key bKey) memoTblGetStmt [toSql ident]
  return $ (not $ Prelude.null count)

descCountMemo :: BoxKey -> String -> IO ()
descCountMemo bKey ident = do
  withRTSSignalsBlocked $ run (key bKey) memoTblDescStmt [toSql ident]
  return ()

delMemo :: BoxKey -> String -> IO ()
delMemo bKey ident = do
  withRTSSignalsBlocked $ run (key bKey) memoTblDelStmt [toSql ident]
  return ()


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
