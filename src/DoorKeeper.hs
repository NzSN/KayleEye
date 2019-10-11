-- file: DoorKeeper

module DoorKeeper where

import Letter
import Homer
import Room
import KayleDefined
import KayleConst
import Modules.ConfigReader
import Time

import Data.Maybe
import qualified Data.Map as Map
import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as Excep

data MaintainElem = MaintainElem { test :: String, status :: String, lastUpdated :: TimeOfDay' } deriving Show
data MaintainTbl = MaintainTbl { tbl :: STM (TVar [MaintainElem]) }

addMElem :: MaintainTbl -> MaintainElem -> STM ()
addMElem mTbl elem = do
  let mTbl_tvar = tbl mTbl

  tvar <- mTbl_tvar
  mTbl_ <- readTVar tvar
  writeTVar tvar $ elem:mTbl_

updateStatus :: MaintainTbl
             -> String -- Test name
             -> String -- New status
             -> STM ()
updateStatus mTbl tName nStatus = do
  let mTbl_tvar = tbl mTbl

  tvar <- mTbl_tvar
  mTbl_ <- readTVar tvar
  writeTVar tvar $ map updateFunc mTbl_

  where updateFunc elem = if test elem == tName
                          then MaintainElem tName nStatus (lastUpdated elem)
                          else elem

updateLast :: MaintainTbl
           -> String -- Test name
           -> TimeOfDay'
           -> STM ()
updateLast mTbl tName tod = do
  let mTbl_tvar = tbl mTbl

  tvar <- mTbl_tvar
  mTbl_ <- readTVar tvar
  writeTVar tvar $ map updateFunc mTbl_

  where updateFunc elem = if test elem == tName
                          then MaintainElem tName (status elem) tod
                          else elem

removeMElem :: MaintainTbl
            -> String -- Test name
            -> STM ()
removeMElem mTbl tName = do
  let mTbl_tvar = tbl mTbl

  tvar <- mTbl_tvar
  mTbl_ <- readTVar tvar
  writeTVar tvar $ removeFunc mTbl_

  where removeFunc (x:xs)
          | (test x) == tName = xs
          | otherwise = x : removeFunc xs

doorKeeper :: KayleEnv -> IO ()
doorKeeper env = do
  let cfgs = envCfg env
      room = envRoom env
      initSeq = 0
      maintainTbl = MaintainTbl $ newTVar []

  -- Master socket
  masterSock <- listenSockGet cfgs
  -- Dispatcher arrived requests
  dispatcher initSeq masterSock maintainTbl

  where dispatcher :: Int -> Socket -> MaintainTbl -> IO ()
        dispatcher i s tbl =
          waitHomer s
          >>= (\h -> forkIO $ job i h tbl)
          >> dispatcher (seqInc i) s tbl

        seqInc i = (i + 1) `mod` 256

        job i h tbl = Excep.handle handler_ (doorKeeperWork i h env tbl)

        handler_ :: SomeException -> IO ()
        handler_ e = return ()

doorKeeperWork :: Int -> Homer -> KayleEnv -> MaintainTbl -> IO ()
doorKeeperWork i h env tbl = do
  -- Prepare stage: Is letter already been processed ? need sync ?
  r <- preparePhase i h env tbl
  if isNothing r
    then return ()
    -- Test result collected
    else collectPhase h env
    -- Disconn letter is arrived trun into terminated phase
         >>= \l -> terminatedPhase l env

-- Assign SeqId and actions depend on event type
preparePhase :: Int -> Homer -> KayleEnv -> MaintainTbl -> IO (Maybe Int)
preparePhase i h env tbl = do
  -- Waiting for the first ask letter
  l_ask <- waitLetter h

  -- Register into maintain table
  let ident_ = ident l_ask
  tod <- getTimeNow

  atomically $ addMElem tbl $ MaintainElem (identWithSeq ident_ i) "Prepare" tod

  -- Deal with request letter
  let event_ = retriFromHeader l_ask "event"
      preFunc e = case e of
                  "merge_request" -> mergePrepare
                  "push"          -> pushPrepare
                  "daily"         -> dailyPrepare

  if isNothing event_
    then return Nothing
    else (preFunc $ fromJust event_) i l_ask h env

mergePrepare :: Int -> Letter -> Homer -> KayleEnv -> IO (Maybe Int)
mergePrepare i l h env =
  (sendLetter h $ ackLetter (ident l) i) >> (return $ Just 0)

pushPrepare :: Int -> Letter -> Homer -> KayleEnv -> IO (Maybe Int)
pushPrepare i l h env =
  sendLetter h (ackLetter (ident l) i) >> (return $ Just 0)

-- Sync: All test of a daily test should carry on the same revision
dailyPrepare :: Int -> Letter -> Homer -> KayleEnv -> IO (Maybe Int)
dailyPrepare i l h env =
  let header = Map.fromList [("event", control_event)]
      content = Map.fromList [("cmd", cmd_noMerged)]
      room = envRoom env

  in putLetter room (Letter (ident l) header content)
     >> getLetter room
     >> sendLetter h (ackLetter (ident l) i) >> (return $ Just 0)

collectPhase :: Homer -> KayleEnv -> IO Letter
collectPhase h env = do
  l <- waitLetter h

  -- Update maintain table

  if typeOfLetter l == disconn_event
    then releaseHomer h >> return l
    -- put push,merge,daily letter into room
    else putLetter (envRoom env) l
         >> collectPhase h env

terminatedPhase :: Letter -> KayleEnv -> IO ()
terminatedPhase l env =
  let terminated = retriFromContent l "who"
        >>= (\who -> return $ terminatedLetter (ident l) who)
        >>= (\terml -> return $ putLetter (envRoom env) terml)
  in return terminated >> return ()

-- Get SeqId from another side
preparePhase' :: Homer -> KayleArgs -> IO Int
preparePhase' h args =
  let reql = reqLetter $ ident2Str $ Identity (target args) (sha args) (event args)
  in sendLetter h reql
     >> waitLetter h
     >>= \l -> let seqId = read (fromJust $ retriFromContent l "seq") :: Int
                in return seqId

terminatePhase' :: String -- ident
                -> Int    -- SeqId
                -> Homer
                -> KayleArgs
                -> IO ()
terminatePhase' ident i h args =
  let disconnl = disconnLetter ident i
  in sendLetter h disconnl >> return ()

listenSockGet :: Configs -> IO Socket
listenSockGet cfgs = do
  let serverOpts = configGet cfgs serverInfoGet serverAddr_err_msg

  (serverAddr:xs) <- getAddrInfo Nothing (Just $ addr serverOpts) (Just $ port serverOpts)

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 10

  return sock

identWithSeq :: String -> Int -> String
identWithSeq ident_ seq = ident_ ++ ":" ++ (show seq)
