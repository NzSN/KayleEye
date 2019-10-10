-- file: Room.hs

module Room where

import Data.Maybe
import Data.Map as Map
import Letter
import Control.Concurrent

type NumOfLetters = Int
type DuplexChannel = (Chan Letter, [MVar Letter])
-- fixme: if there is many many letters in a Room
--        in real sence then Room should be implement
--        the queue via MVar but not a list.
data Room = Room { inEntry :: Chan Letter,
                   outEntry :: MVar (Map String DuplexChannel) }
          | Empty_Room

maxNumOfLetters = 100

newRoom :: IO Room
newRoom = do
  in_ <- newChan
  out_ <- newEmptyMVar
  return $ Room in_ out_

isRoomEmpty :: Room -> IO Bool
isRoomEmpty r = return True

unRegister :: String -> Room -> IO ()
unRegister ident_ r = do
  let channels = outEntry r

  map_ <- takeMVar channels
  putMVar channels (delete ident_ map_)

register :: String -> Room -> IO (Either DuplexChannel (MVar Letter))
register ident_ r = do
  let out_ = outEntry r

  map_ <- takeMVar out_

  if member ident_ map_
    then newEmptyMVar >>= \mvar ->
                            let reg = insert ident_ (regChannel (theChannel map_) mvar) map_
                            in putMVar out_ reg >> (return $ Right mvar)
    else newChan >>= \chan ->
                       let dChan = (chan, [])
                           regedMap = insert ident_ dChan map_
                       in putMVar out_ regedMap >> (return $ Left dChan)

  where
    theChannel map_ = fromJust $ Map.lookup ident_ map_

    regChannel :: DuplexChannel -> MVar Letter -> DuplexChannel
    regChannel dChan ml = (fst dChan, (ml:snd dChan))

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
    else writeChan (fst . fromJust $ qMaybe) l
         >> putMVar channels map_

-- Use by DoorKeeper
putLetter' :: Room -> Letter -> IO ()
putLetter' r l = writeChan (inEntry r) l
