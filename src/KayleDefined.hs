module KayleDefined where

import Test.HUnit
import Debug.Trace

import Control.Concurrent.STM
import System.Environment
import Modules.ConfigReader
import Network.HTTP.Client
import Homer
import Notifier
import LetterBox
import Room
import Time
import Puller
import Data.Map as Map

import Control.Concurrent

type Args = [String]

type CmdName = String
type ItemName = String

type CtrlItem = [String]
type CtrlSubEnv = Map String CtrlItem
type CtrlEnv = Map String CtrlSubEnv

subCtrlEnv :: CtrlEnv -> CmdName -> Maybe CtrlSubEnv
subCtrlEnv env cmd = Map.lookup cmd env

ctrlItem :: CtrlSubEnv -> ItemName -> Maybe CtrlItem
ctrlItem env iname = Map.lookup iname env

subEnvInsert :: CtrlEnv -> CmdName -> CtrlSubEnv -> CtrlEnv
subEnvInsert env cName subEnv = insert cName subEnv env

itemSearch :: CtrlEnv -> CmdName -> ItemName -> Maybe CtrlItem
itemSearch env cName iName =
  Map.lookup cName env >>= \subEnv -> Map.lookup iName subEnv

itemInsert :: CtrlEnv -> CmdName -> ItemName -> CtrlItem -> CtrlEnv
itemInsert env cName iName item =
  update subEnvInsert cName env

  where subEnvInsert :: CtrlSubEnv -> Maybe CtrlSubEnv
        subEnvInsert subEnv = Just $ Map.insert iName item subEnv

itemUpdate :: CtrlEnv -> CmdName -> ItemName -> CtrlItem -> CtrlEnv
itemUpdate env cName iName item =
  update subEnvUpdate cName env

  where subEnvUpdate :: CtrlSubEnv -> Maybe CtrlSubEnv
        subEnvUpdate subEnv = Just $ Map.update (\item_ -> Just item) iName subEnv

itemDelete :: CtrlEnv -> CmdName -> ItemName -> CtrlEnv
itemDelete env cName iName =
  update subEnvDelete cName env

  where subEnvDelete subEnv = Just $ delete iName subEnv

itemDeleteAll :: CtrlEnv -> ItemName -> CtrlEnv
itemDeleteAll env iName = mapWithKey itemDrop env
  where itemDrop k a = Map.delete iName a

type Event = String
data RegisterItem = RegItem { regIdent :: String, regStatus :: String, tod :: TimeOfDay' } deriving (Show, Eq)
newtype RegisterItems = RegItems { regItems :: [RegisterItem] } deriving (Show, Eq)
data RegisterBlock = RegBlk { regBlk :: Map String RegisterItems } deriving (Show, Eq)
data RegisterTbl = RegTbl { regTbl :: TVar (Map Event RegisterBlock) }

register_status = "register"
unRegister_status = "unRegister"
finished_status = "finished"

newRegisterTbl :: STM RegisterTbl
newRegisterTbl = newTVar Map.empty >>= return . RegTbl

addEvent :: RegisterTbl -> String -> STM ()
addEvent rTbl event =
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.insert event (RegBlk Map.empty) tbl)
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

getEvent :: RegisterTbl -> String -> STM (Maybe RegisterBlock)
getEvent rTbl e = (readTVar $ regTbl rTbl) >>= \tbl -> return $ Map.lookup e tbl

addBlock :: RegisterTbl
            -> String -- Event name
            -> String -- Block name
            -> STM ()
addBlock rTbl e b =
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.update event_update e tbl)
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

  where event_update block = return . RegBlk $ Map.insert b (RegItems []) (regBlk block)

getBlock :: RegisterTbl
         -> String -- Event name
         -> String -- Block name
         -> STM (Maybe RegisterItems)
getBlock rTbl e b =
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.lookup e tbl
                >>= \block -> Map.lookup b $ regBlk block)

addItem :: RegisterTbl
        -> String -- Event name
        -> String -- Block name
        -> RegisterItem
        -> STM ()
addItem rTbl e b item =
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.update item_update e tbl)
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

  where item_update block = Just . RegBlk $ Map.update item_update' b (regBlk block)
        item_update' items =
          let itemArray = regItems items
          in if elem item itemArray
             then Just . RegItems $ itemArray
             else Just . RegItems $ item:itemArray

iStatusChange :: RegisterTbl
         -> String -- Event
         -> String -- Block
         -> String -- Item
         -> String -- Status
         -> TimeOfDay'
         -> STM ()
iStatusChange rTbl e b i status tod =
  (readTVar $ regTbl rTbl)
  >>= \tbl -> (return $ Map.update register_handle e tbl)
  >>= \tbl -> writeTVar (regTbl rTbl) tbl

  where register_handle block = Just . RegBlk $ Map.update register_handle' b (regBlk block)
        register_handle' items = let itemArray = regItems items
                                 in Just . RegItems $ Prelude.map register_map_func itemArray
        register_map_func item = if regIdent item == i
                                 then RegItem (regIdent item) status tod
                                 else item

register :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> TimeOfDay'
           -> STM ()
register rTbl e b i tod = iStatusChange rTbl e b i register_status tod


unregister :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> TimeOfDay'
           -> STM ()
unregister rTbl e b i tod = iStatusChange rTbl e b i unRegister_status tod

markFinished :: RegisterTbl
           -> String -- Event
           -> String -- Block
           -> String -- Item
           -> TimeOfDay'
           -> STM ()
markFinished rTbl e b i tod = iStatusChange rTbl e b i finished_status tod



data KayleEnv = KayleEnv { envCfg :: Configs,
                           envMng :: Manager,
                           envArgs :: Args,
                           envHomer :: Homer,
                           envKey :: BoxKey,
                           envRoom :: Room,
                           envPuller :: Puller,
                           envControl :: CtrlEnv,
                           envNotifier :: Notifier (String, String) }

kayleEnvSetBKey :: KayleEnv -> BoxKey -> KayleEnv
kayleEnvSetBKey (KayleEnv cfg mng args homer key room puller cEnv notifier) b =
  KayleEnv cfg mng args homer b room puller cEnv notifier

kayleEnvSetHomer :: KayleEnv -> Homer -> KayleEnv
kayleEnvSetHomer (KayleEnv cfg mng args homer key room puller cEnv notifier) h =
  KayleEnv cfg mng args h key room puller cEnv notifier

kayleEnvSetCEnv :: KayleEnv -> CtrlEnv -> KayleEnv
kayleEnvSetCEnv (KayleEnv cfg mng args homer key room puller cEnv notifier) cEnv_ =
  KayleEnv cfg mng args homer key room puller cEnv_ notifier


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

-- Test cases
definedTest :: Test
definedTest = TestList [TestLabel "Defined testing" (TestCase definedAssert)]
  where definedAssert :: Assertion
        definedAssert = do
          let env = empty

          let new_env = subEnvInsert env "1" empty
          assertEqual "" True (member "1" new_env)

          let new_env1 = itemInsert new_env "1" "GL8900" ["1", "2", "3"]
          assertEqual "" (Just ["1", "2", "3"]) $ itemSearch new_env1 "1" "GL8900"

          let new_env2 = itemUpdate new_env1 "1" "GL8900" ["2", "3", "4"]
          assertEqual "" (Just ["2", "3", "4"]) $ itemSearch new_env2 "1" "GL8900"

          let new_env3 = itemDelete new_env2 "1" "GL8900"
          assertEqual "" (Nothing) $ itemSearch new_env3 "1" "GL8900"

          let new_env4 = itemInsert new_env "1" "GL8900" ["1", "2", "3"]
              new_env5 = subEnvInsert new_env4 "2" empty
              new_env6 = itemInsert new_env5 "2" "GL8900" ["2", "3", "4"]
              new_env7 = itemDeleteAll new_env6 "GL8900"
          assertEqual "" (Just ["1", "2", "3"]) $ itemSearch new_env6 "1" "GL8900"
          assertEqual "" (Just ["2", "3", "4"]) $ itemSearch new_env6 "2" "GL8900"
          assertEqual "" (Nothing) $ itemSearch new_env7 "1" "GL8900"
          assertEqual "" (Nothing) $ itemSearch new_env7 "2" "GL8900"

          return ()
