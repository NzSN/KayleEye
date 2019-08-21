-- file: LetterBox.hs

module LetterBox where

import Database.HDBC
import Database.HDBC.MySQL
import Data.ByteString.Lazy.Internal

import Homer
import Types

-- Prepared Statements
databaseCreateStmt :: String
tableCreateStmt :: String
searchLetterStmt :: String
searchLetterStmt = "SELECT content FROM ? where proj=? AND ident=?"

data BoxKey = BoxKey { key :: Connection }

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
searchLetter :: BoxKey
             -> String -- Project name
             -> String -- Identity
             -> IO ByteString
searchLetter key_ proj ident_ = do
  letters <- quickQuery' (key key_) searchLetterStmt [toSql "current", toSql proj, toSql ident_]
  return $ fromSql $ head . head $ letters
