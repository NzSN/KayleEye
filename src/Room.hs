-- file: Room.hs

module Room where

import Test.HUnit

import Data.Maybe
import Data.Map as Map
import Letter
import Control.Concurrent

type NumOfLetters = Int
type DuplexChannel = Chan Letter
data Room = Room { inEntry :: Chan Letter,
                   outEntry :: MVar (Map String DuplexChannel),
                   outEntry' :: Maybe DuplexChannel}
          | Empty_Room

maxNumOfLetters = 100

isEmptyRoom :: Room -> Bool
isEmptyRoom Empty_Room = True
isEmptyRoom _ = False

newRoom :: IO Room
newRoom = do
  in_ <- newChan
  out_ <- newMVar Map.empty
  return $ Room in_ out_ Nothing

isRoomEmpty :: Room -> IO Bool
isRoomEmpty r = return True

unRegister :: String -> Room -> IO ()
unRegister ident_ r = do
  let channels = outEntry r

  map_ <- takeMVar channels
  putMVar channels (delete ident_ map_)

register :: String -> Room -> IO (Maybe Room)
register ident_ r = do
  let out_ = outEntry r

  map_ <- takeMVar out_

  if member ident_ map_
    -- Receiver exists, register failed
    then putMVar out_ map_ >> return Nothing
    -- No DuplexChannel is register with that identifier just register
    else newChan >>= \chan ->
                       let regedMap = insert ident_ chan map_
                       in putMVar out_ regedMap
                          >> (return $ Just $ Room (inEntry r) (outEntry r) (Just chan))

-- Use by KayleHome
getLetter :: Room -> IO Letter
getLetter r = readChan (inEntry r)

putLetter :: Room -> Letter -> IO ()
putLetter r l = do
  let ident_ = ident l
      channels = outEntry r

  map_ <- takeMVar $ channels

  let qMaybe = Map.lookup ident_ map_

  if isNothing qMaybe
    then putMVar channels map_
    else writeChan (fromJust qMaybe) l
         >> putMVar channels map_

-- Use by DoorKeeper
putLetter' :: Room -> Letter -> IO ()
putLetter' r l = writeChan (inEntry r) l

getLetter' :: Room -> IO Letter
getLetter' r =
  let channel = fromJust $ outEntry' r
  in readChan channel

-- Room Unit Testing
roomUnitTest :: Test
roomUnitTest = TestList [TestLabel "Room Unit Testing" (TestCase roomUnitAssert)]

roomUnitAssert :: Assertion
roomUnitAssert = do
  room <- newRoom

  wait1 <- newEmptyMVar
  wait2 <- newEmptyMVar

  forkIO $ master room wait1
  forkIO $ slave room wait2

  takeMVar wait1
  takeMVar wait2

  where
    master :: Room -> MVar () -> IO ()
    master room lock = do

      letter <- getLetter room
      assertEqual "Master Received" "123" (ident letter)

      putLetter room letter

      putMVar lock ()

    slave :: Room -> MVar () -> IO ()
    slave room lock = do
      let l = Letter "123" Map.empty Map.empty

      rMayb <- register "123" room

      maybe (assertEqual "Register failed" 0 1)
        (\r -> putLetter' r l
               >> getLetter' r
               >>= assertEqual "Slave Received" "123" . ident) rMayb

      putMVar lock ()
