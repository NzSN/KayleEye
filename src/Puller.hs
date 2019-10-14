-- file: Puller.hs

module Puller where

import RepoOps
import Time
import Modules.ConfigReader
import Notifier

import Control.Monad
import Control.Concurrent
import Network.HTTP.Client

data Puller = Puller { requests :: Chan String,
                       locker :: MVar (),
                       manager :: Manager,
                       config :: Configs,
                       notifier :: Notifier (String, String)}

isLocked :: Puller -> IO Bool
isLocked p = do
  isUnlocked <- isEmptyMVar $ locker p
  return $ not $ isUnlocked

isUnlocked :: Puller -> IO Bool
isUnlocked p = do
  b <- isLocked p
  return $ not b

lock :: Puller -> IO ()
lock p = putMVar (locker p) ()

unlock :: Puller -> IO ()
unlock p = takeMVar (locker p)

newPuller :: Manager -> Configs -> Notifier (String, String) -> IO Puller
newPuller m c n = do
  chan <- newChan
  mvar <- newEmptyMVar
  return $ Puller chan mvar m c n

pullRequest :: Puller -> String -> IO ()
pullRequest p iid =
  let rq = requests p
  in writeChan rq iid

pullerSpawn :: Puller -> IO ()
pullerSpawn p = forever $ do
  let locker_ = locker p
      reqQ    = requests p

      manager_ = manager p
      configs = config p
      notifier_ = notifier p

  iid <- readChan reqQ

  lock p >> accept manager_ configs notifier_ iid >> unlock p
