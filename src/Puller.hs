-- file: Puller.hs

module Puller where

import RepoOps
import Time
import Modules.ConfigReader
import Notifier
import KayleConst

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

      interval = configGet configs pullTimeGet "Pull time is not configured"

      beginTime = fst $ pullTime interval
      endTime   = snd $ pullTime interval

  iid <- readChan reqQ
  print $ "Pull request " ++ iid ++ " arrived"

  now <- getTimeNow
  if isTimeInInterval now beginTime endTime
    -- If Current time is in the interval present in configuration
    -- just accept the merge requests
    then print "Try Pulling"
         >> lock p
         >> print "Pulling"
         >> accept manager_ configs notifier_ iid
         >> unlock p
    -- If Current time is not in the interval present in configuration
    -- then wait until in the interval
    else print "Wait until in interval to pulling"
         >> (threadDelay $ (beginTime Time.- now) * minute_micro)
         >> lock p
         >> accept manager_ configs notifier_ iid
         >> unlock p
