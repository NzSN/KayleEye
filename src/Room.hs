-- file: Room.hs

module Room where

import Homer
import Control.Concurrent

type NumOfLetters = Int
-- fixme: if there is many many letters in a Room
--        in real sence then Room should be implement
--        the queue via MVar but not a list.
type Room = MVar (NumOfLetters, [Letter])
type RoomGuard = MVar ()

maxNumOfLetters = 100

isRoomEmpty :: Room -> IO Bool
isRoomEmpty r = takeMVar r >>= return . null

-- Retrive letter from head of queue
getLetter :: Room -> IO Letter
getLetter r =
  takeMVar r
  >>= \(n, (x:xs)) ->
        if n > 0
        then putMVar r (n-1, xs) >> return x
        else return Empty_letter

-- Push letter into queue
putLetter :: Room -> Letter -> IO ()
putLetter r l =
  takeMVar r
  >>= \(n, lq) ->
        if n + 1 > maxNumOfLetters
        then return ()
        else putMVar r $ (n+1, lq ++ [l])
