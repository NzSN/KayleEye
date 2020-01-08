-- file: Notifier.hs

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}

{-# LANGUAGE OverloadedStrings #-}

module Notifier where

import Test.HUnit
import Debug.Trace

import Homer
import Modules.ConfigReader
import KayleConst
import KayleBasics
import Time

import Data.Maybe
import Control.Concurrent

import Data.Text.Internal.Lazy as Lazy (Text)
import Data.Text.Internal as Internal

import Network.Mail.Mime (Mail)
import Network.Mail.SMTP

import Data.String.Conversions (cs)

import qualified Control.Exception as Excep

class MessageFormat a where
  present :: a -> [String]

instance MessageFormat String where
  present a = [a]

instance (MessageFormat a, MessageFormat b) => MessageFormat (a, b) where
  present (a, b) = present a ++ present b

newtype Message a = Message { msg :: a } deriving Show
data NotifyUnit a = NotifyUnit { event :: Int, message :: Message a } deriving Show
data Notifier a = Notifier { queue :: Chan (NotifyUnit a), configs :: Configs }

type MessageHandler a = NotifyUnit a -> Configs -> IO ()

type Mode = Int
emailMode = 0

procUnit = 5

newNotifier :: MessageFormat a => Configs -> IO (Notifier a)
newNotifier cfg = do
  chan <- newChan
  return $ Notifier chan cfg

-- Init function of Notifier module this function may
-- possible be called in forkIO.
notifierSpawn :: MessageFormat a => Notifier a -> IO ()
notifierSpawn n =
  let cfgs = configs n
      interval = configGet cfgs notifyTimeGet "Notify time is not configured"

      beginTime = fst $ notifyTime interval
      endTime = snd $ notifyTime interval

  in notifyloop beginTime endTime

  where

    notifyUntil n msg =
      Excep.handle (\(Excep.SomeException e) -> threadDelay (minute_micro `div` 60)
                                                >> notifyUntil n msg)
      (notifyTo' n msg)

    notifyloop bTime eTime = do
      msg <- (readChan $ queue n)
      now <- getTimeNow

      if isTimeInInterval now bTime eTime
        then notifyUntil n msg
        else (threadDelay $ (bTime Time.- now) * minute_micro)
             >> notifyUntil n msg

      notifyloop bTime eTime


notifyViaEmail :: Notifier (String, String)
               -> String -- Subject
               -> String -- Body
               -> IO ()
notifyViaEmail n sub body =
  notify n $ NotifyUnit 0 $ Message (sub, body)

notify :: Notifier a -> NotifyUnit a -> IO ()
notify (Notifier chan cfgs) nu = writeChan chan nu

-- Take procUnit number of notifies and immediatly
-- put left notifies back to Notifier then deal l-
-- etters be taken.
notifyTo :: MessageFormat a => Notifier a -> IO ()
notifyTo n = (readChan $ queue n) >>= \x -> (handlerSelect n) x $ configs n

notifyTo' :: MessageFormat a => Notifier a -> NotifyUnit a -> IO ()
notifyTo' n x = (handlerSelect n) x $ configs n

-- Temporarily only support email.
handlerSelect :: MessageFormat a => Notifier a -> MessageHandler a
handlerSelect n = emailNotify

emailMsgCreate :: [String] -> Message (String, String)
emailMsgCreate infos = Message ((head infos), (last infos))

-- With the use of this message handler caller
-- should be make sure about the message list
-- is in a form (Subject, Body)
emailNotify :: MessageFormat a => NotifyUnit a -> Configs -> IO ()
emailNotify nu cfgs = do
  let adminEmail = adminEmailGet cfgs
      extraEmails_ = extraEmailsGet cfgs

  maybe (return ()) (sendEmails . (\x -> [x]) . adminEmailAddr) adminEmail
  maybe (return ()) (sendEmails . extraEmails) extraEmails_

  where
    sendEmails addrs =
      let sendTo = addrs
          message_ = msg . message $ nu
          subject = head . present $ message_
          body = last . present $ message_

          emails = map (\addr -> emailCreate subject body addr cfgs) sendTo
      in mapM_ (\email -> emailSend email cfgs) emails

emailCreate :: String -- Subject
            -> String -- Body
            -> String -- Address
            -> Configs -> Mail
emailCreate subject_ body_ addr cfgs =
  let mailInfo = fromJust $ emailInfoGet $ cfgs
      user_ = user mailInfo
      address = cs addr :: Internal.Text
      from = Address Nothing (cs user_ :: Internal.Text)
      to = [Address (Just "GPON") address]
      cc = []
      bcc = []
      subject = cs subject_
      body = plainTextPart (cs body_ :: Lazy.Text)
  in simpleMail from to cc bcc subject [body]


emailSend :: Mail -> Configs -> IO ()
emailSend e cfgs =
  let mailInfo = fromJust $ emailInfoGet $ cfgs
      hostName = host mailInfo
      user_ = user mailInfo
      pass_ = pass mailInfo
  in sendMailWithLogin hostName user_ pass_ e

-- Test cases
notifierTest :: Test
notifierTest = TestList [TestLabel "Notifier Test" (TestCase notifierAssert)]
  where notifierAssert :: Assertion
        notifierAssert = do
          cfgs <- loadConfig "GL8900" "./config"
          notifier <- newNotifier cfgs

          notifyViaEmail notifier "Test" "123"
          notifyTo notifier
