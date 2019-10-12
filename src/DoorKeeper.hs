-- file: DoorKeeper

module DoorKeeper where

import Test.HUnit

import Letter
import Homer
import Room
import KayleDefined
import KayleConst
import Modules.ConfigReader
import Time

import Data.List as List
import Data.Maybe
import Data.Bool
import qualified Data.Map as Map
import Network.Socket
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as Excep

import Debug.Trace

data MaintainElem = MaintainElem { testID :: String,
                                   status :: String,
                                   lastUpdated :: TimeOfDay' }
                  | Empty_Elem deriving (Show, Eq)

data MaintainTbl = MaintainTbl { tbl :: TVar (Map.Map String [MaintainElem]) }

newMaintainTbl :: STM MaintainTbl
newMaintainTbl = do
  tbl <- newTVar Map.empty
  return $ MaintainTbl tbl

newMElemWithTime :: String -> String -> IO MaintainElem
newMElemWithTime tID status = do
  tod <- getTimeNow
  return $ MaintainElem tID status tod

addMElem :: MaintainTbl -> MaintainElem -> STM ()
addMElem mTbl elem = do
  let mTbl_tvar = tbl mTbl

  map_ <- readTVar mTbl_tvar

  let key = identWithoutSeq (testID elem)
      newMap = bool
        (Map.insert key (fromJust $ addFunc []) map_)
        (Map.update addFunc key map_)
        (Map.member key map_)

  writeTVar mTbl_tvar newMap

  where addFunc elems = Just $ if notElem elem elems
                               then elem:elems
                               else let newL = List.delete elem elems
                                    in elem:newL

searchMElem :: MaintainTbl
            -> String -- Test name
            -> STM (Maybe MaintainElem)
searchMElem mTbl tName = do
  map_ <- readTVar (tbl mTbl)

  return $ Map.lookup key map_
    >>= \elems ->
          Just $ foldl (\acc x -> bool acc x (testID x == tName)) Empty_Elem elems

  where key = identWithoutSeq tName

updateStatus :: MaintainTbl
             -> String -- Test name
             -> String -- New status
             -> STM ()
updateStatus mTbl tName nStatus = do
  let mTbl_tvar = tbl mTbl

  map_ <- readTVar mTbl_tvar

  let key = identWithoutSeq tName
      newMap = Map.update (\elems -> Just $ map updateFunc elems) key map_
  writeTVar mTbl_tvar newMap

  where updateFunc elem =
          let newElem = MaintainElem tName nStatus (lastUpdated elem)
          in bool elem newElem (testID elem == tName)

updateLast :: MaintainTbl
           -> String -- Test name
           -> TimeOfDay'
           -> STM ()
updateLast mTbl tName tod = do
  let mTbl_tvar = tbl mTbl

  map_ <- readTVar mTbl_tvar

  let key = identWithoutSeq tName
      newMap = Map.update (\elems -> Just $ map updateFunc elems) key map_
  writeTVar mTbl_tvar newMap

  where updateFunc elem =
          let newElem = MaintainElem tName (status elem) tod
          in bool elem newElem (testID elem == tName)

removeMElem :: MaintainTbl
            -> String -- Test name
            -> STM Int
removeMElem mTbl tName = do
  let mTbl_tvar = tbl mTbl

  map_ <- readTVar mTbl_tvar

  let key = identWithoutSeq tName
      newMap = Map.update (\elems -> Just $ removeFunc elems) key map_
      elemsMayb = Map.lookup key newMap
      pMayb = elemsMayb >>= \elems -> if List.length elems == 0
                                      then Just $ (1, Map.delete key newMap)
                                      else Just $ (0, newMap)

  maybe (return 0) (\p -> writeTVar mTbl_tvar (snd p) >> (return $ fst p)) pMayb

  where removeFunc (x:xs)
          | (testID x) == tName = xs
          | otherwise = x : removeFunc xs

doorKeeper :: KayleEnv -> IO ()
doorKeeper env = do
  let cfgs = envCfg env
      room = envRoom env
      initSeq = 0
      maintainTbl = newMaintainTbl

  -- Master socket
  masterSock <- listenSockGet cfgs
  -- Dispatcher arrived requests
  tbl <- atomically $ maintainTbl
  dispatcher initSeq masterSock tbl

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

  maybe (return ()) nextPhase r

  where
    -- Test result collected
    nextPhase _ = collectPhase h env tbl
    -- Disconn letter is arrived trun into terminated phase
                >>= \l -> terminatedPhase l env tbl

-- Assign SeqId and actions depend on event type
preparePhase :: Int -> Homer -> KayleEnv -> MaintainTbl -> IO (Maybe Int)
preparePhase i h env tbl = do
  -- Waiting for the first ask letter
  l_ask <- waitLetter h

  -- Deal with request letter
  let event_ = retriFromHeader l_ask "event"
      preFunc e = case e of
                  "merge_request" -> mergePrepare
                  "push"          -> pushPrepare
                  "daily"         -> dailyPrepare

  maybe (return Nothing) (\event -> preFunc event i l_ask h env tbl) event_

mergePrepare :: Int -> Letter -> Homer -> KayleEnv -> MaintainTbl -> IO (Maybe Int)
mergePrepare i l h env tbl =
  (sendLetter h $ ackLetter (ident l) i) >> (return $ Just 0)

pushPrepare :: Int -> Letter -> Homer -> KayleEnv -> MaintainTbl -> IO (Maybe Int)
pushPrepare i l h env tbl =
  sendLetter h (ackLetter (ident l) i) >> (return $ Just 0)

-- Sync: All test of a daily test should carry on the same revision
dailyPrepare :: Int -> Letter -> Homer -> KayleEnv -> MaintainTbl -> IO (Maybe Int)
dailyPrepare i l h env tbl =
  let room = envRoom env
      ident_ = ident l
  in getTimeNow
     -- Register into maintain table
     >>= (\tod -> atomically $ addMElem tbl $ MaintainElem (identWithSeq ident_ (show i)) "Online" tod)
     -- Blocking Puller
     >> putLetter room (mergeBlockLetter ident_)
     -- Wait KayleHome blocking Puller
     >> getLetter room
     -- Ack to the request from client
     >> sendLetter h (ackLetter ident_ i) >> (return $ Just 0)

collectPhase :: Homer -> KayleEnv -> MaintainTbl -> IO Letter
collectPhase h env tbl = do
  l <- waitLetter h

  if typeOfLetter l == disconn_event
    then releaseHomer h >> return l
    -- put push,merge,daily letter into room
    else putLetter (envRoom env) l
         >> collectPhase h env tbl

terminatedPhase :: Letter -> KayleEnv -> MaintainTbl -> IO ()
terminatedPhase l env tbl =
  let seqMaybe = retriFromHeader l "seq"
      room = (envRoom env)
      termLetter = terminatedLetter (ident l) ""
      terminated seq = atomically $ removeMElem tbl (identWithSeq (ident l) seq)
  in maybe (return 0) terminated seqMaybe
     >>= \code -> bool (return ()) (putLetter room termLetter) (code == 0)

-- Get SeqId from another side
preparePhase' :: Homer -> KayleArgs -> IO Int
preparePhase' h args =
  let reql = reqLetter $ ident2Str $ Identity (target args) (sha args) (event args)
  in sendLetter h reql
     >> waitLetter h
     -- No seqId infor in the ack letter so the server maybe incomplete
     -- just throw and error.
     >>= \l -> maybe (error "Error format")
               -- return the seqId retrive from the ack letter just received
               (\seq -> return (read seq :: Int)) (retriFromContent l "seq")

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

identWithSeq :: String -> String -> String
identWithSeq ident_ seq = ident_ ++ ":" ++ seq

identWithoutSeq :: String -> String
identWithoutSeq ident_ =
  let array = identSplit ident_
  in ident2Str $ Identity (array !! 0) (array !! 1) (array !! 2)

-- Doorkeeper Unit Testing
doorKeeperUnitTest :: Test
doorKeeperUnitTest = TestList [TestLabel "DoorKeeper UnitTesting" (TestCase doorKeeperAssert)]
  where
    doorKeeperAssert :: Assertion
    doorKeeperAssert = do

      -- IdentWithSeq and identWithoutSeq
      let testStr = "GL8900:12345678:merge_request"
          withSeq = identWithSeq testStr  "1"
      assertEqual "IdentWithSeq" (testStr ++ ":1") withSeq

      assertEqual "IdentWithoutSeq" testStr (identWithoutSeq withSeq)

      -- MaintainTbl Testing
      tbl <- atomically $ newMaintainTbl
      newElem <- newMElemWithTime withSeq "Online"

      atomically $ addMElem tbl $ newElem
      elem <- atomically $ searchMElem tbl withSeq
      print elem
      assertEqual "Search Maintain Table" withSeq $
        maybe ("") (\elem_ -> testID elem_) elem
