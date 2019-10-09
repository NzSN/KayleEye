-- file: Puller.hs

module Puller where

import RepoOps
import KayleDefined

import Control.Monad
import Control.Concurrent
import Network.HTTP.Client

data Puller = Puller { requests :: Chan String, locker :: MVar () }

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

pullerSpawn :: Puller -> KayleEnv -> IO ()
pullerSpawn p env = forever $ do
  let locker_ = locker p
      reqQ    = requests p
      manager_ = envMng env
      configs = envCfg env

  lock p

  iid <- readChan reqQ
  accept manager_ configs iid

  unlock p
