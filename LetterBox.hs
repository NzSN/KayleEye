-- file: LetterBox.hs

module LetterBox where

import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class

import Database.HDBC
import Database.HDBC.MySQL

import Data.Maybe
import Data.ByteString.Lazy.Internal

import Homer
import Types


procTbl :: String
procTbl = "processing"

historyTbl :: String
historyTbl = "history"

-- Prepared Statements
databaseCreateStmt :: String
databaseCreateStmt = "CREATE DATABASE kayleHome"

tableCreateStmt :: String
tableCreateStmt = "CREATE TABLE ? (" ++
                  "ident VARCHAR(256)," ++
                  "content VARCHAR(255)," ++
                  "PRIMARY KEY(ident))"

contentUpdateStmt :: String
contentUpdateStmt = "UPDATE current" ++
                   "SET content = ?" ++
                   "WHERE ident = ?"

searchLetterStmt :: String
searchLetterStmt = "SELECT proj,content,status,ident FROM ? WHERE ident = ?"

data BoxKey = BoxKey { key :: Connection }

boxInit :: BoxKey -> IO ()
boxInit key_ = do
  let conn = key key_
  run conn tableCreateStmt [toSql procTbl]
  run conn tableCreateStmt [toSql historyTbl]
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
  run (key key_) "INSERT INTO ? VALUES (?, ?)"
    [toSql tblN, toSql (ident letter), toSql (content letter)]
  commit (key key_)

searchLetter :: BoxKey
             -> String -- Identity
             -> MaybeT IO Letter
searchLetter key_ ident_ = MaybeT $ do
  letters <- quickQuery' (key key_) searchLetterStmt [toSql procTbl, toSql ident_]
  if not null letters
    then return $ Just $ head $ map toLetter letters
    else return Nothing

  where toLetter :: [SqlValue] -> Letter
        toLetter [sqlIdent, sqlContent] =
          Letter (fromSql sqlIdent) (fromSql sqlContent)

removeLetter :: BoxKey
             -> String -- Table name
             -> String -- Identity
             -> IO ()
removeLetter key_ tbl ident_ = do
  run (key key_) "DELETE FROM ? WHERE ident = ?" [toSql tbl, toSql ident_]
  commit (key key_)

isLetterExists :: BoxKey
               -> String -- Identity
               -> IO Bool
isLetterExists key_ ident_ = do
  letter <- runMaybeT $ searchLetter key_ ident_
  if null letter
    then return True
    else return False

updateLetter :: BoxKey
             -> String -- Identity
             -> String -- Content
             -> Bool   -- Is test done ? yes,then move to history table
             -> IO Int
updateLetter key_ ident_ content status = do
  letter <- runMaybeT $ searchLetter key_ ident_

  if isNothing letter
    then return 1
    else let letter_ = fromJust $ letter
         in if status == True
            then (insertLetter key_ historyTbl (Letter (ident letter_) content))
                 >> removeLetter key_ procTbl (ident letter_)
                 >> commit (key key_) >> return 0
            else run (key key_) contentUpdateStmt [toSql content, toSql (ident letter_)]
                 >> commit (key key_) >> return 0
