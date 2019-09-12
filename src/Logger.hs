-- file: Logger.hs

module Logger where

import Test.HUnit

import System.IO
import Control.Monad.Trans.Reader
import Control.Monad.Writer

type Logger = WriterT [String] IO

seperator :: String
seperator = " : "

runLogger :: Logger a -> String -> IO ()
runLogger l path = do
  (_, logMsgs) <- runWriterT l
  if null logMsgs
    then return ()
    else do file <- openFile path AppendMode
            mapM_ (hPutStrLn file) logMsgs
            hClose file

appendLogger :: String -- Header
             -> String -- Content
             -> Logger ()
appendLogger h c = tell [h ++ seperator ++ c]



-- Test Cases
loggerTest :: Test
loggerTest = TestList [TestLabel "Logger put" (TestCase loggerAssert)]
  where loggerAssert :: Assertion
        loggerAssert = do
          let l = do
                tell ["hello"]
                (return 6 :: Logger Int)
          c <- runWriterT l

          assertEqual "LoggerPut"  True (6 == (fst c) && "hello" == (head $ snd c))
