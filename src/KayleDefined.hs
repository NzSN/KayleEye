module KayleDefined where

import Modules.ConfigReader
import Network.HTTP.Client
import Homer
import LetterBox
import Room
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

itemInsert :: CtrlEnv -> CmdName -> ItemName -> CtrlItem -> CtrlEnv
itemInsert env cName iName item =
  update subEnvUpdate cName env

  where subEnvUpdate :: CtrlSubEnv -> Maybe CtrlSubEnv
        subEnvUpdate subEnv = Just $ update (\item_old -> Just item) iName subEnv

itemDelete :: CtrlEnv -> CmdName -> ItemName -> CtrlEnv
itemDelete env cName iName =
  update subEnvUpdate cName env

  where subEnvUpdate subEnv = Just $ delete iName subEnv


data KayleEnv = KayleEnv { envCfg :: Configs,
                           envMng :: Manager,
                           envArgs :: Args,
                           envHomer :: Homer,
                           envKey :: BoxKey,
                           envRoom :: Room,
                           envPullerLocker :: MVar (),
                           envControl :: CtrlEnv }

kayleEnvSetBKey :: KayleEnv -> BoxKey -> KayleEnv
kayleEnvSetBKey (KayleEnv cfg mng args homer key room locker cEnv) b =
  KayleEnv cfg mng args homer b room locker cEnv

kayleEnvSetHomer :: KayleEnv -> Homer -> KayleEnv
kayleEnvSetHomer (KayleEnv cfg mng args homer key room locker cEnv) h =
  KayleEnv cfg mng args h key room locker cEnv

kayleEnvSetCEnv :: KayleEnv -> CtrlEnv -> KayleEnv
kayleEnvSetCEnv (KayleEnv cfg mng args homer key room locker cEnv) cEnv_ =
  KayleEnv cfg mng args homer key room locker cEnv_


data KayleArgs = KayleArgs {
  proj :: String,
  target :: String,
  sha :: String,
  iid :: String,
  configPath :: String,
  cmds :: String,
  -- Event's value is "push" or "merge_request"
  event :: String } deriving Show


