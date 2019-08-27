-- file: Logger.hs

module Logger where

import Test.HUnit

import System.IO
import Control.Monad.Trans.Reader
import Control.Monad.Writer

type LoggerConfigs = [String]
type LoggerContent = String

type Logger = WriterT LoggerContent (ReaderT LoggerConfigs IO)

putToLogger :: Logger a -> LoggerContent -> Logger a
putToLogger l c = tell c >> l

-- Test Cases
loggerTest :: Test
loggerTest = TestList [TestLabel "Logger put" (TestCase loggerAssert)]
  where loggerAssert :: Assertion
        loggerAssert = do
          let l = do
                (return 6 :: Logger Int)
              c = runReaderT . runWriterT $ l

          ((c ["123"]) >>= print)

          assertEqual "LoggerPut" 1 1
