-- file: Room.hs

module Room where

import Letter
import Control.Concurrent

type NumOfLetters = Int
-- fixme: if there is many many letters in a Room
--        in real sence then Room should be implement
--        the queue via MVar but not a list.
data Room = Room { inEntry :: Chan Letter, outEntry :: Chan Letter } | Empty_Room

maxNumOfLetters = 100

newRoom :: IO Room
newRoom = do
  in_ <- newChan
  out_ <- newChan
  return $ Room in_ out_

isRoomEmpty :: Room -> IO Bool
isRoomEmpty r = return True

roomReverse :: Room -> Room
roomReverse r = Room (outEntry r) (inEntry r)

-- Retrive letter from head of queue
getLetter :: Room -> IO Letter
getLetter r = readChan (inEntry r)

-- Push letter into queue
putLetter :: Room -> Letter -> IO ()
putLetter r l = writeChan (outEntry r) l
