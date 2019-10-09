module KayleDefined where

import Modules.ConfigReader
import Network.HTTP.Client
import Homer
import LetterBox
import Room

type Args = [String]
data KayleEnv = KayleEnv { envCfg :: Configs,
                           envMng :: Manager,
                           envArgs :: Args,
                           envHomer :: Homer,
                           envKey :: BoxKey,
                           envRoom :: Room }

data KayleArgs = KayleArgs {
  proj :: String,
  target :: String,
  sha :: String,
  iid :: String,
  configPath :: String,
  cmds :: String,
  -- Event's value is "push" or "merge_request"
  event :: String } deriving Show


