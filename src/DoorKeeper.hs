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

data ProcInfo = ProcInfo { procId :: IdentStr, procSubTest :: String } deriving Show

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

  let key = (testID elem)
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

  where key = tName

updateStatus :: MaintainTbl
             -> String -- Test name
             -> String -- New status
             -> STM ()
updateStatus mTbl tName nStatus = do
  let mTbl_tvar = tbl mTbl

  map_ <- readTVar mTbl_tvar

  let key = tName
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

  let key = tName
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

  let key = tName
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
  dispatcher masterSock

  where dispatcher :: Socket -> IO ()
        dispatcher s =
          waitHomer s
          >>= (\h -> forkIO $ job h)
          >> dispatcher s

        job h = Excep.handle handler_ (doorKeeperWork h env)

        handler_ :: SomeException -> IO ()
        handler_ e = return ()

doorKeeperWork :: Homer -> KayleEnv -> IO ()
doorKeeperWork h env = do
  -- Prepare stage: Is letter already been processed ? need sync ?
  r <- preparePhase h env

  maybe (return ()) nextPhase r

  where
    -- Test result collected
    nextPhase pInfo = collectPhase pInfo h env
    -- Disconn letter is arrived trun into terminated phase
                >>= \l -> terminatedPhase l env

-- Assign SeqId and actions depend on event type
preparePhase :: Homer -> KayleEnv -> IO (Maybe ProcInfo)
preparePhase h env = do
  -- Waiting for the first ask letter
  l_req <- waitLetter h

  -- Deal with request letter
  let subTest = retriFromHeader l_req "who"

  maybe (return Nothing) (\sub -> register l_req sub) subTest

  where register req_l who =
          let regLetter req sub = registerLetter (ident req) sub
              ident_ = ident req_l
          -- Register the subTest
          in (putLetter (envRoom env) $ regLetter req_l who)
             -- Waiting for registering
             >> getLetter (envRoom env)
             -- Giva an ack to Kayle
             >>= \answer -> (if isAnswerOK answer
                              -- If register success then add the connection to maintain table
                            then (sendLetter h $ ackLetter ident_ True)
                            else (sendLetter h $ ackLetter ident_ False))
                            -- If the request is rejected by KayleHome then just return Nothing
                            -- to exit from the thread.
                            >> (bool (return Nothing) (return $ Just $ ProcInfo ident_ who)
                                 $ isAnswerOK answer)

collectPhase :: ProcInfo -> Homer -> KayleEnv -> IO Letter
collectPhase pInfo h env = do
  -- If connection is interrupted just unregister the sub test
  l <- withGuard interruptedHandler $ waitLetter h

  if typeOfLetter l == disconn_event
    then releaseHomer h >> return l
    -- put push,merge,daily letter into room
    else putLetter (envRoom env) l
         >> collectPhase pInfo h env

  where interruptedHandler (SomeException e) =
          let ident_ = procId pInfo
              subTest = procSubTest pInfo
              unregLetter = unregisterLetter ident_ subTest
              room = envRoom env
          in putLetter room unregLetter
             >> fail "Connection interrupted"

terminatedPhase :: Letter -> KayleEnv -> IO ()
terminatedPhase l env =
  let subTestMaybe = retriFromContent l "who"
      room = (envRoom env)
  in notifyKayle subTestMaybe

  where notifyKayle whoMaybe =
          maybe (return ())
          (\who -> putLetter (envRoom env) $ termLetter who)
          whoMaybe

        termLetter who = terminatedLetter (ident l) who

-- Get SeqId from another side
preparePhase' :: Homer -> KayleArgs -> IO Int
preparePhase' h args =
  let ident_ = ident2Str $ Identity (target args) (sha args) (event args)
      reql = reqLetter ident_ (target args)
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
  let disconnl = disconnLetter ident i (target args)
  in sendLetter h disconnl >> return ()

listenSockGet :: Configs -> IO Socket
listenSockGet cfgs = do
  let serverOpts = configGet cfgs serverInfoGet serverAddr_err_msg

  (serverAddr:xs) <- getAddrInfo Nothing (Just $ addr serverOpts) (Just $ port serverOpts)

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 10

  return sock

-- Doorkeeper Unit Testing
doorKeeperUnitTest :: Test
doorKeeperUnitTest = TestList [TestLabel "DoorKeeper UnitTesting" (TestCase doorKeeperAssert)]
  where
    doorKeeperAssert :: Assertion
    doorKeeperAssert = do

      -- IdentWithSeq and identWithoutSeq
      let testStr = "GL8900:12345678:merge_request"
          withSeq = testStr
      assertEqual "IdentWithSeq" (testStr ++ ":1") withSeq

      assertEqual "IdentWithoutSeq" testStr withSeq

      -- MaintainTbl Testing
      tbl <- atomically $ newMaintainTbl
      newElem <- newMElemWithTime withSeq "Online"

      atomically $ addMElem tbl $ newElem
      elem <- atomically $ searchMElem tbl withSeq
      print elem
      assertEqual "Search Maintain Table" withSeq $
        maybe ("") (\elem_ -> testID elem_) elem
