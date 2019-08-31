-- Kayle, a program to do judgement of quality of code, accept or reject.
-- Arguments : ProjectName, iid, SHA

{-#LANGUAGE OverloadedStrings #-}

module Main where

import Debug.Trace

-- Process, File, Directory
import System.Environment
import Data.Map
import Data.Maybe

-- Configuration
import Modules.ConfigReader as C
import Control.Monad.Reader

-- Constants
import KayleConst
import KayleBasics hiding (notify)

import Homer

type Revision = String
type JudgeContent = String

data KayleArgs = KayleArgs {
  proj :: String,
  target :: String,
  sha :: String,
  iid :: String } deriving Show

getKayleArgs :: IO KayleArgs
getKayleArgs = do
  args <- getArgs
  return $ KayleArgs (head args)
    (head . tail $ args)
    (last $ args)
    (head . tail . tail $ args)

main :: IO ()
main = let c args = (loadConfig cfile configPath)
                    >>= (\config -> (print config) >> (return (args, config)) ) >>= f
             where cfile = (proj args) ++ "_" ++ (target args)

           f p = let serverOpts = configGet (snd p) serverInfoGet serverAddr_err_msg
                     -- Testing
                     testing h = (judge $ fromJust $ testCmdGet (snd p))
                                 >>= (\isPass -> notify h (fst p) isPass)
                 in pickHomer (addr serverOpts) (port serverOpts) >>= testing
  -- Get arguments and configurations
       in getKayleArgs >>= c

-- Accept if pass test otherwise throw an error
notify :: Homer -> KayleArgs -> Bool -> IO ()
notify h args False = notify' h args (fromList [(target args, "F")])
notify h args True = notify' h args (fromList [(target args, "T")])

notify' :: Homer -> KayleArgs -> Map String String -> IO ()
notify' homer args c = let i = ident2Str $ Identity (proj args) (sha args)
                           h = fromList [("iid", (iid args))]
                           l = Letter i h c
                       in homerFlyWith homer l >> return ()

judge :: TestContent_cfg -> IO Bool
judge TestContent_None = return True
judge c = do
  let cmds = C.content c

  isSuccess <- run_command_1 (head $ cmds)
  isSubSuccess <- case Prelude.null $ tail cmds of
                    True -> judge TestContent_None
                    False -> judge $ TestContent_cfg (tail cmds)

  return $ isSuccess && isSubSuccess
