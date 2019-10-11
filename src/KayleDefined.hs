module KayleDefined where

import Test.HUnit
import Debug.Trace

import System.Environment
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
