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

data ProcInfo = ProcInfo { procId :: IdentStr,
                           procSubTest :: String,
                           procRoom :: Room,
                           procHomer :: Homer }

doorKeeper :: KayleEnv -> IO ()
doorKeeper env = do
  let cfgs = envCfg env
      room = envRoom env
      initSeq = 0

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
    nextPhase pInfo = collectPhase pInfo
    -- Disconn letter is arrived trun into terminated phase
                >>= \l -> terminatedPhase l pInfo

-- Assign SeqId and actions depend on event type
preparePhase :: Homer -> KayleEnv -> IO (Maybe ProcInfo)
preparePhase h env = do
  -- Waiting for the first request letter
  l_req <- waitLetter h

  let subTest = retriFromHeader l_req "who"

  -- Register a channel
  roomM <- maybe (return Nothing)
           (\ident_ -> Room.register ident_ (envRoom env)) (registerKey l_req)

  -- Deal with request letter
  let sub = fromMaybe "" subTest
      room = fromMaybe Empty_Room roomM

  if sub == "" || isEmptyRoom room
    then return Nothing
    else registerToKayle l_req sub room

  where registerToKayle req_l who newRoom =
          let regLetter req sub = registerLetter (ident req) sub
              ident_ = ident req_l
          -- Register the subTest
          in (putLetter' newRoom $ regLetter req_l who)
             -- Waiting for registering
             >> getLetter' newRoom
             -- Giva an ack to Kayle
             >>= \answer -> (if isAnswerOK answer
                              -- If register success then add the connection to maintain table
                            then (sendLetter h $ ackAcceptLetter ident_)
                            else (sendLetter h $ ackRejectedLetter ident_))
                            -- If the request is rejected by KayleHome then just return Nothing
                            -- to exit from the thread.
                            >> (bool (return Nothing) (return $ Just $ ProcInfo ident_ who newRoom h)
                                 $ isAnswerOK answer)

        registerKey :: Letter -> Maybe String
        registerKey letter = retriFromContent letter "who"
                             >>= \who -> return $ (ident letter) ++ ":" ++ who

collectPhase :: ProcInfo -> IO Letter
collectPhase pInfo = do
  let h = procHomer pInfo

  -- If connection is interrupted just unregister the sub test
  l <- withGuard interruptedHandler $ waitLetter h

  if typeOfLetter l == disconn_event
    then releaseHomer h >> return l
    -- put push,merge,daily letter into room
    else putLetter (procRoom pInfo) l
         >> collectPhase pInfo

  where interruptedHandler (SomeException e) =
          let ident_ = procId pInfo
              subTest = procSubTest pInfo
              unregLetter = unregisterLetter ident_ subTest
              room = procRoom pInfo
          in putLetter room unregLetter
             >> fail "Connection interrupted"

terminatedPhase :: Letter -> ProcInfo -> IO ()
terminatedPhase l pInfo =
  let subTestMaybe = retriFromContent l "who"
      room = (procRoom pInfo)
  in notifyKayle subTestMaybe

  where notifyKayle whoMaybe =
          maybe (return ())
          (\who -> putLetter (procRoom pInfo) $ termLetter who)
          whoMaybe

        termLetter who = terminatedLetter (ident l) who

-- Get SeqId from another side
preparePhase' :: Homer -> KayleArgs -> IO Bool
preparePhase' h args =
  let ident_ = ident2Str $ Identity (target args) (sha args) (event args)
      reql = reqLetter ident_ (target args)
  in sendLetter h reql
     >> waitLetter h
     -- No seqId infor in the ack letter so the server maybe incomplete
     -- just throw and error.
     >>= \l -> maybe (return False) isAccepted (retriFromContent l "answer")

  where isAccepted answer = return $ answer == "Accepted"

terminatePhase' :: String -- ident
                -> Homer
                -> KayleArgs
                -> IO ()
terminatePhase' ident_ h args =
  let disconnl = disconnLetter ident_ (target args)
  in sendLetter h disconnl >> return ()

listenSockGet :: Configs -> IO Socket
listenSockGet cfgs = do
  let serverOpts = configGet cfgs serverInfoGet serverAddr_err_msg

  (serverAddr:xs) <- getAddrInfo Nothing (Just $ addr serverOpts) (Just $ port serverOpts)

  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  bind sock (addrAddress serverAddr)
  listen sock 10

  return sock
