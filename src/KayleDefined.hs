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


