-- file: Notifier.hs

module Notifier where

import Homer
import NotifyHandler
import Modules.ConfigReader

import Control.Concurrent

type Event = String
type Message = [String]
type NotifyUnit = (Event, Message)
type Notifier = MVar (Int, [NotifyUnit])

data NotifierConfig = NotifierConfig {
  notifier :: Notifier,
  configs  :: Configs }

type MessageHandler = NotifyUnit -> IO ()

type Mode = Int
emailMode = 0

procUnit = 5

notify :: Notifier -> NotifyUnit -> IO ()
notify n nu = takeMVar n >>= \(num, q) -> putMVar n $ (num+1, q ++ [nu])

-- Take procUnit number of notifies and immediatly
-- put left notifies back to Notifier then deal l-
-- etters be taken.
doNotify :: Notifier -> MessageHandler -> IO ()
doNotify n h =
  takeMVar n
  >>= (\(num, q) ->
        if num < procUnit
        then putMVar n (0, [])
             >> return q
        else let notifies = take procUnit q
             in (putMVar n (num - procUnit, drop procUnit q))
                >> return notifies)
  >>= \units -> mapM_ h units

handlerSelect :: Mode -> MessageHandler
handlerSelect 0 = emailNotify

notifierSpawn :: NotifierConfig -> IO ()
notifierSpawn n m = return ()
