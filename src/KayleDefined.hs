module KayleDefined where

import Test.HUnit
import Debug.Trace

import Control.Concurrent.STM
import System.Environment
import Modules.ConfigReader
import Network.HTTP.Client
import Homer
import Notifier
import Letter
import KayleConst
import LetterBox
import Room
import Time
import Puller

import Data.Maybe as Maybe
import Data.Map as Map
import Data.List as List

import Control.Concurrent

type Args = [String]

type Event = String
data RegisterItem = RegItem { regIdent :: String, regStatus :: String, tod :: TimeOfDay' } deriving (Show, Eq)
newtype RegisterItems = RegItems { regItems :: [RegisterItem] } deriving (Show, Eq)
data RegisterBlock = RegBlk { regBlk :: Map String RegisterItems } deriving (Show, Eq)
data RegisterTbl = RegTbl { regTbl :: TVar (Map Event RegisterBlock) }

register_status = "register"
unRegister_status = "unRegister"
finished_status = "finished"

newRegisterTbl :: IO RegisterTbl
newRegisterTbl = atomically $ newTVar Map.empty >>= return . RegTbl

showRegTable :: RegisterTbl -> IO ()
showRegTable rTbl = do
  tbl <- atomically $ (readTVar $ regTbl rTbl)
  print tbl

addBlock :: RegisterTbl -> String -> IO ()
addBlock rTbl event = atomically $
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.insert event (RegBlk Map.empty) tbl)
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

getBlock :: RegisterTbl -> String -> IO (Maybe RegisterBlock)
getBlock rTbl e = atomically $ (readTVar $ regTbl rTbl) >>= \tbl -> return $ Map.lookup e tbl

addItems :: RegisterTbl
            -> String -- Event name
            -> String -- Block name
            -> IO ()
addItems rTbl e b = atomically $
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.update event_update e tbl)
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

  where event_update block = return . RegBlk $ Map.insert b (RegItems []) (regBlk block)

removeItems :: RegisterTbl
            -> String -- Event name
            -> String -- Block name
            -> IO ()
removeItems rTbl e b = atomically $
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.update event_update e tbl)
  >>= \ tbl -> writeTVar (regTbl rTbl) tbl

  where event_update block = return . RegBlk $ Map.delete b $ regBlk block

getItems :: RegisterTbl
         -> String -- Event name
         -> String -- Block name
         -> IO (Maybe RegisterItems)
getItems rTbl e b = atomically $
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.lookup e tbl
                >>= \block -> Map.lookup b $ regBlk block)

addItem :: RegisterTbl
        -> String -- Event name
        -> String -- Block name
        -> RegisterItem
        -> IO ()
addItem rTbl e b item = atomically $
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.update item_update e tbl)
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

  where item_update block = Just . RegBlk $ Map.update item_update' b (regBlk block)
        item_update' items =
          let itemArray = regItems items
          in if elem item itemArray
             then Just . RegItems $ itemArray
             else Just . RegItems $ item:itemArray

addItem' :: RegisterTbl
        -> String -- Event name
        -> String -- Block name
        -> RegisterItem
        -> IO ()
addItem' rTbl e b item = atomically $
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (if isNothing $ Map.lookup e tbl
              then (return $ Map.insert e (RegBlk $ Map.insert b (RegItems [item]) Map.empty) tbl)
              else (return $ Map.update blkProc e tbl))
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

  where
    blkProc :: RegisterBlock -> Maybe RegisterBlock
    blkProc blk =
      let blk_ = regBlk blk
      in if isNothing $ Map.lookup b blk_
         then (Just $ RegBlk $ Map.insert b (RegItems [item]) blk_)
         else (Just $ RegBlk $ Map.update itemsProc b blk_)

    itemsProc :: RegisterItems -> Maybe RegisterItems
    itemsProc items =
      let items_ = regItems items
      in if elem item items_
         then Just items
         else Just $ RegItems $ item:items_

getItem :: RegisterTbl
        -> String -- Event
        -> String -- Block
        -> String -- Item
        -> IO (Maybe RegisterItem)
getItem rTbl e b i = atomically $ getItem_atom rTbl e b i

getItem_atom :: RegisterTbl
        -> String -- Event
        -> String -- Block
        -> String -- Item
        -> STM (Maybe RegisterItem)
getItem_atom rTbl e b i = do
  tbl <- (readTVar $ regTbl rTbl)

  let itemMay = Map.lookup e tbl
                >>= \blk -> (Map.lookup b $ regBlk blk)
                            >>= \items -> (find (\item -> regIdent item == i) $ regItems items)

  return $ maybe Nothing (\item -> return item) itemMay


isItemExists :: RegisterTbl
             -> String -- Event
             -> String -- Block
             -> String -- Item
             -> IO Bool
isItemExists rTbl e b i = atomically $ do
  exists <- getItem_atom rTbl e b i
  return $ maybe False (\_ -> True) exists

iStatusChange :: RegisterTbl
         -> String -- Event
         -> String -- Block
         -> String -- Item
         -> String -- Status
         -> IO ()
iStatusChange rTbl e b i status = do
  tod <- getTimeNow
  atomically $ (readTVar $ regTbl rTbl)
    >>= \tbl -> (return $ Map.update (\block -> register_handle tod block) e tbl)
                >>= \tbl -> writeTVar (regTbl rTbl) tbl

  where register_handle tod block =
          Just . RegBlk $ Map.update (\items -> register_handle' tod items) b (regBlk block)
        register_handle' tod items =
          let itemArray = regItems items
          in Just . RegItems $ Prelude.map (\item -> register_map_func tod item ) itemArray
        register_map_func tod item =
          if regIdent item == i
          then RegItem (regIdent item) status tod
          else item

register :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> IO ()
register rTbl e b i = iStatusChange rTbl e b i register_status

unregister :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> IO ()
unregister rTbl e b i = iStatusChange rTbl e b i unRegister_status

markFinished :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> IO ()
markFinished rTbl e b i = iStatusChange rTbl e b i finished_status

isRegister :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> IO Bool
isRegister rTbl e b i = do
  item <- getItem rTbl e b i

  return $ maybe False (\i -> regStatus i == register_status) item

isUnRegister :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> IO Bool
isUnRegister rTbl e b i = do
  item <- getItem rTbl e b i

  return $ maybe False (\i -> regStatus i == unRegister_status) item


data KayleEnv = KayleEnv { envCfg :: Configs,
                           envMng :: Manager,
                           envArgs :: Args,
                           envHomer :: Homer,
                           envKey :: BoxKey,
                           envRoom :: Room,
                           envPuller :: Puller,
                           envRegTbl :: RegisterTbl,
                           envNotifier :: Notifier (String, String) }

kayleEnvSetBKey :: KayleEnv -> BoxKey -> KayleEnv
kayleEnvSetBKey (KayleEnv cfg mng args homer key room puller cEnv notifier) b =
  KayleEnv cfg mng args homer b room puller cEnv notifier

kayleEnvSetHomer :: KayleEnv -> Homer -> KayleEnv
kayleEnvSetHomer (KayleEnv cfg mng args homer key room puller cEnv notifier) h =
  KayleEnv cfg mng args h key room puller cEnv notifier

kayleEnvSetCEnv :: KayleEnv -> RegisterTbl -> KayleEnv
kayleEnvSetCEnv (KayleEnv cfg mng args homer key room puller rTbl notifier) rTbl_ =
  KayleEnv cfg mng args homer key room puller rTbl_ notifier

kayleEnvSetRoom :: KayleEnv -> Room -> KayleEnv
kayleEnvSetRoom (KayleEnv cfg mng args homer key room puller rTbl notifier) room_ =
  KayleEnv cfg mng args homer key room_ puller rTbl notifier


data KayleArgs = KayleArgs {
  proj :: String,
  target :: String,
  sha :: String,
  iid :: String,
  configPath :: String,
  cmds :: String,
  -- Event's value is "push" or "merge_request"
  event :: String} deriving Show

getKayleArgs :: IO KayleArgs
getKayleArgs = do
  args <- getArgs
  return $ KayleArgs
    ((!!) args 0) -- proj
    ((!!) args 1) -- target
    ((!!) args 2) -- sha
    ((!!) args 3) -- iid
    ((!!) args 4) -- configPath
    ((!!) args 5) -- build cmds
    ((!!) args 6) -- isMr

-- Cautions: You should make sure the letter is a
-- control letter which contain who info in content
identWithSubTest :: Letter -> String
identWithSubTest l = (ident l) ++ (maybe "" (\sub -> ":" ++ sub) $ retriFromContent l content_who)

-- Test cases
definedTest :: Test
definedTest = TestList [TestLabel "Defined testing" (TestCase definedAssert)]
  where definedAssert :: Assertion
        definedAssert = do
          regTable <- newRegisterTbl

          addItem' regTable "Merge" "GL8900" $ RegItem "T1" unRegister_status Empty_Tod
          KayleDefined.register regTable "Merge" "GL8900" "T1"

          itemMay <- getItem regTable "Merge" "GL8900" "T1"
          let item = fromJust itemMay

          assertEqual "Register" "T1" $ regIdent item
          assertEqual "Register" register_status $ regStatus item

          unregister regTable "Merge" "GL8900" "T1"
          itemMay' <- getItem regTable "Merge" "GL8900" "T1"

          let item' = fromJust itemMay'
          assertEqual "Register" "T1" $ regIdent item'
          assertEqual "Register" unRegister_status $ regStatus item'

          return ()
