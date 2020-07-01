module RepoOps where

{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Data.Aeson
import Data.Aeson.Types

import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import KayleConst
import Modules.ConfigReader as Config

import Data.ByteString.Lazy

import Data.Maybe

import System.Environment
import KayleBasics

import Data.String.Conversions (cs)
import Control.Concurrent.Thread.Delay

import Text.Regex.TDFA

import Data.Time.Clock
import Data.Time.Calendar

import Notifier

import GitLab
import GitLab.Types
import GitLab.API.Projects

acceptUrl = "http://10.5.4.211:8011/api/v4/projects/*/merge_requests/*/merge?private_token=*"
rebaseUrl = "http://10.5.4.211:8011/api/v4/projects/*/merge_requests/*/rebase?private_token=*"
commitsUrl = "http://10.5.4.211:8011/api/v4/projects/*/repository/commits/*?private_token=*"
mergesUrl = "http://10.5.4.211:8011/api/v4/projects/*/merge_requests?private_token=*"
singleMRUrl = "http://10.5.4.211:8011/api/v4/projects/*/merge_requests/*?private_token=*"

-- Accept an merge request into main branch
accept :: Manager -> Configs -> Notifier (String, String)
       -> String -- iid, Merge request id
       -> IO ()
accept mng cfgs notifier iid = do
  let token = priToken $ configGet cfgs priTokenGet "PrivateToken not found"
      projId = projID $ configGet cfgs listProjConfig "Project info not found"
      acceptUrl_ = replaceAll acceptUrl [projId, iid, token]
  code <- put_req acceptUrl_ mng

  case code of
    -- If merge request is unable to be accepted (ie: Work in Progress,
    -- Closed, Pipeline Pending Completion, or Failed while requiring Success) -
    -- you’ll get a 405 and the error message ‘Method Not Allowed’
    405 -> notifyViaEmail notifier "Failed to accept"
           ("Merge request " ++ iid ++ " is unable to be accepted")
    -- If it has some conflicts and can not be merged - you’ll get a 406 and
    -- the error message ‘Branch cannot be merged’
    406 -> rebase mng cfgs notifier iid
           -- try accept after rebase if rebase success accept should be success too
           -- otherwise a message will be notified in rebase function.
          >>= \returncode -> if returncode == 0
                             then accept mng cfgs notifier iid
                             else notifyViaEmail notifier "Failed to accept" "Rebase failed"
    -- If the sha parameter is passed and does not match the HEAD of the source -
    -- you’ll get a 409 and the error message ‘SHA does not match HEAD of source branch’
    409 -> notifyViaEmail notifier "Failed to accept"
           ("Merge request " ++ iid ++ ": SHA does not match HEAD of source branch")
    -- If you don’t have permissions to accept this merge request - you’ll get a 401
    401 -> notifyViaEmail notifier "failed" "Permissions denied"
    -- Merge request not found or the url is wrong
    404 -> notifyViaEmail notifier "Failed to accept" "Merge requests not found or the url is wrong"
    -- Success
    200 -> print "accept success" >> return ()
    _   -> notifyViaEmail notifier "Failed to accept" $ "Error : " ++ (show code)

-- Rebase merge request to target branch
rebase :: Manager -> Configs -> Notifier (String, String)
       -> String -- iid, Merge request id
       -> IO Int
rebase mng cfgs notifier iid = do
  let token = priToken $ configGet cfgs priTokenGet "PrivateToken not found"
      projId = projID $ configGet cfgs listProjConfig "Project info not found"
      rebaseUrl_ = replaceAll rebaseUrl [projId, iid, token]
  code <- put_req rebaseUrl_ mng

  case code of
    -- If you don’t have permissions to push to the merge request’s source branch -
    -- you’ll get a 403 Forbidden response.
    403 -> notifyViaEmail notifier "Failed to rebase" "Permission denied to rebase merge request" >> return (-1)
    -- The API will return a 202 Accepted response if the request is enqueued successfully
    -- Merge request not found or the url is wrong
    404 -> notifyViaEmail notifier "Failed to rebase" ("Merge requests(" ++ iid ++ ") not found or the url is wrong")
           >> return (-1)
    -- Success
    202 -> print "rebase success" >> return 0
    _   -> (notifyViaEmail notifier "Failed to rebase" $ "Error : " ++ (show code)) >> return (-1)

-- Commit Message
commitMsg :: Manager -> Configs -> String -> IO String
commitMsg m cfg sha = do
  let token = priToken $ configGet cfg priTokenGet "PrivateToken not found"
      projId = projID $ configGet cfg listProjConfig "Project info not found"
      commitUrl_ = replaceAll commitsUrl [projId, sha, token]

  response <- get_req commitUrl_ m

  case (fst response) of
    200 -> return $ getMsg (snd response)
    _   -> return ""
  where
    getMsg :: ByteString -> String
    getMsg l = do
      let o = (decode l :: Maybe Object)
      if isNothing o
        then ""
        else fromJust $ flip parseMaybe (fromJust o) $ \obj -> do
               msg <- obj .: cs "message"
               return msg

-- Merge Request info
data MergeRequestInfo = MRInfo { author_username :: String }
mergeRequestInfo :: Manager
                 -> Configs
                 -> String -- Merge request iid
                 -> IO MergeRequestInfo
mergeRequestInfo m c iid = do
  let token = priToken $ configGet c priTokenGet "PrivateToken not found"
      projId = projID $ configGet c listProjConfig "Project info not found"
      mrInfoUrl_ = replaceAll singleMRUrl [projId, iid, token]

  response <- get_req mrInfoUrl_ m

  case (fst response) of
    200 -> return $ MRInfo $ getMRInfo (snd response)
    _ -> return $ MRInfo ""

  where getMRInfo body = do
          let content = (decode body :: Maybe Object)

              author :: Maybe String
              author = flip parseMaybe (fromJust content) $ \obj -> do
                author <- obj .: cs "author"

                let username = fromJust $ flip parseMaybe author $ \obj -> do
                      username <- obj .: cs "username"
                      return username
                return username

          fromJust author

-- Merge Request daily
subMatch :: (a, b, c, d) -> d
subMatch (a, b, c, d) = d

mrIid :: (a, b, c) -> a
mrIid (a, b, c) = a

mrTitle :: (a, b, c) -> b
mrTitle (a, b, c) = b

mrDate :: (a, b, c) -> c
mrDate (a, b, c) = c

regex_gitlab_date_pattern :: String
regex_gitlab_date_pattern = "([0-9]+)-([0-9]+)-([0-9]+)"

regex_gitlab_date :: String -> [String]
regex_gitlab_date dateStr =
  let result = dateStr =~ regex_gitlab_date_pattern :: (String, String, String, [String])
  in subMatch result

regex_gitlab_year :: [String] -> String
regex_gitlab_year date = Prelude.head $ date

regex_gitlab_month :: [String] -> String
regex_gitlab_month date = Prelude.head . Prelude.tail $ date

regex_gitlab_day :: [String] -> String
regex_gitlab_day date = Prelude.last $ date

isYesterday :: [String] -> [String] -> Bool
isYesterday today anotherDay =
  let year_today = read (regex_gitlab_year today) :: Int
      month_today = read (regex_gitlab_month today) :: Int
      day_today = read (regex_gitlab_day today) :: Int

      year_another = read (regex_gitlab_year anotherDay) :: Int
      month_another = read (regex_gitlab_month anotherDay) :: Int
      day_another = read (regex_gitlab_day anotherDay) :: Int

  in if anotherDay == []
     then False
     else (year_today) - (year_another) == 0 &&
          (month_today - month_today) == 0 &&
          (day_today - day_another) == 1

yesterday'sMR :: Manager -> Configs -> IO String
yesterday'sMR m c = do
  let token = priToken $ configGet c priTokenGet "PrivateToken not found"
      projId = projID $ configGet c listProjConfig "Project info not found"
      mergeUrl_ = replaceAll mergesUrl [projId, token]

  time <- getCurrentTime
  let date' = regex_gitlab_date $ showGregorian $ utctDay time

  response <- get_req mergeUrl_ m

  case (fst response) of
    200 -> return $ getMRs_yesterday date' (snd response)
    _ -> return ""

  where
    getMRs_yesterday :: [String] -> ByteString -> String
    getMRs_yesterday date' l = do
      let mrs = (decode l :: Maybe [Object])
      if isNothing mrs
        then ""
        else let mrInfos = Prelude.map mrInfoTrans (fromJust mrs)
             in Prelude.foldl
                (\acc x -> let dateMerged = regex_gitlab_date (mrDate x)
                               iid = (show $ mrIid x)
                               title = mrTitle x
                               isYesterday' = isYesterday date' dateMerged
                           in if isYesterday'
                              then acc ++ iid ++ " : " ++ title ++ "\n"
                              else acc)
                "" mrInfos

mrInfoTrans :: Object -> (Int, String, String)
mrInfoTrans o = let result = flip parseMaybe o $ \obj -> do
                        a  <- obj .: cs "iid"
                        b  <- obj .: cs "title"
                        c  <- obj .: cs "merged_at"
                        return (a, b, c)
                    in if isNothing result
                        then (0, "N/A", "N/A")
                        else fromJust result

-- Unit Testing
repoTest :: Test
repoTest = TestList [TestLabel "Repo Testing" (TestCase repoAssert)]
  where repoAssert :: Assertion
        repoAssert = do
          m <- newManager defaultManagerSettings
          c <- loadConfig "GL8900" "./config"

          info <- mergeRequestInfo m c "200"

          print $ author_username info
