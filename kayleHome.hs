-- KayleHome Collect judgements of kayle and then execute if these judgements is right.

{-# LANGUAGE OverloadedStrings #-}

import Debug.Trace

-- Http Request
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

-- Json
import Data.Aeson as Aeson
import Data.Maybe
import Data.Aeson.Types
import Data.List.NonEmpty
import Data.Either
import Data.Text.Internal.Lazy as Lazy (Text)
import Data.Text.Internal as Internal
import Data.String.Conversions (cs)

-- Process, File, Directory
import System.Process as Process
import Control.Exception
import System.IO
import System.Directory
import System.Environment
import Control.Concurrent.Thread.Delay

-- Email
import Network.Mail.SMTP

-- Exception
import Control.Exception.Base
import System.IO.Error

-- Configuration
import Modules.ConfigReader


accept :: Manager -> Configs -> IO ()
accept mng cfgs = do
  args <- getArgs

  let acceptUrl = replace_iid (Prelude.head $ configSearch cfgs "AcceptUrl") (Prelude.last args)
  code <- put_req acceptUrl mng

  case code of
    -- If merge request is unable to be accepted (ie: Work in Progress,
    -- Closed, Pipeline Pending Completion, or Failed while requiring Success) -
    -- you’ll get a 405 and the error message ‘Method Not Allowed’
    405 -> notify "Merge request is unable to be accepted" cfgs
    -- If it has some conflicts and can not be merged - you’ll get a 406 and
    -- the error message ‘Branch cannot be merged’
    406 -> rebase mng cfgs >> delay (3 * seconds_micro) >> accept mng cfgs
    -- If the sha parameter is passed and does not match the HEAD of the source -
    -- you’ll get a 409 and the error message ‘SHA does not match HEAD of source branch’
    409 -> notify "SHA does not match HEAD of source branch" cfgs
    -- If you don’t have permissions to accept this merge request - you’ll get a 401
    401 -> notify "Permissions denied" cfgs
    -- Merge request not found or the url is wrong
    404 -> notify "Merge requests not found or the url is wrong" cfgs
    -- Success
    200 -> return ()

rebase :: Manager -> Configs -> IO ()
rebase mng cfgs = do
  args <- getArgs
  let rebaseUrl = replace_iid (Prelude.head $ configSearch cfgs "RebaseUrl") (Prelude.last args)
  code <- put_req rebaseUrl mng

  case code of
    -- If you don’t have permissions to push to the merge request’s source branch -
    -- you’ll get a 403 Forbidden response.
    403 -> notify "Permission denied to rebase merge request" cfgs
    -- The API will return a 202 Accepted response if the request is enqueued successfully
    -- Merge request not found or the url is wrong
    404 -> notify "Merge requests not found or the url is wrong" cfgs
    -- Success
    202 -> return ()

put_req :: String -> Manager -> IO Int
put_req url mng = do
  initialRequest <- parseRequest url
  response <- httpLbs (initialRequest { method = "PUT" }) mng
  return $ statusCode $ responseStatus response

notify :: Lazy.Text -> Configs -> IO ()
notify content cfgs = do
  let mailInfo = fromJust $ emailInfoGet $ cfgs
      host = Prelude.head $ mailInfo
      user = Prelude.head . Prelude.tail $ mailInfo
      pass = Prelude.last $ mailInfo

      adminEmail = (cs $ fromJust $ adminEmailGet $ cfgs) :: Internal.Text

  let from = Address Nothing ((cs user) :: Internal.Text)
      to   = [Address (Just "admin") adminEmail]
      cc   = []
      bcc  = []
      subject = "CI informations"
      body    = plainTextPart content

  let mail = simpleMail from to cc bcc subject [body]
  sendMailWithLogin host user pass mail


