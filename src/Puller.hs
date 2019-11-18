-- file: Puller.hs

module Puller where

import RepoOps
import Time
import Modules.ConfigReader
import Notifier
import KayleConst

import Data.Maybe
import Data.Foldable

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
  let reqQ = requests p

  iid <- readChan reqQ
  print $ "Pull request " ++ iid ++ " arrived"

  -- Auto merge condition check
  cond <- pullCond p iid

  if cond
    then doPulling p iid
    else return ()

  where
    -- DistrustList checking
    -- fixme: author name is better to retrive from worker but
    --        not directly request from gitlab.
    distrustList_check :: Puller -> String -> IO Bool
    distrustList_check p iid = do
      let manager_ = manager p
          config_ = config p

      -- Is the merge requests's user in distrust list
      mrInfo <- mergeRequestInfo manager_ config_ iid

      let author = author_username mrInfo
          distrustList = distrustListGet config_

          list = if isNothing distrustList
                 then []
                 else distrust_list $ fromJust distrustList
      return $ not . elem author $ list

    -- True if this merge request is able to processed.
    pullCond :: Puller -> String -> IO Bool
    pullCond p iid =
      -- If an item return True means checking is passed
      -- otherwise checking is failed.
      let condArray = [distrustList_check]
      in foldlM checking True condArray

      where
        checking acc cond = do
          val <- cond p iid
          return $ val && acc

doPulling :: Puller -> String -> IO ()
doPulling p iid = do
  let configs = config p
      interval = configGet configs pullTimeGet "Pull time is not configured"

      manager_ = manager p
      notifier_ = notifier p

      beginTime = fst $ pullTime interval
      endTime   = snd $ pullTime interval

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
