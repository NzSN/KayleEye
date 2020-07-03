-- | Utils

module Utils where

import Test.HUnit
import Control.Exception
import Control.Concurrent
import KayleConst (second_micro_int)
import KayleBasics (put_req)
import Control.DeepSeq

import Network.HTTP.Client

-- Only can deal with IO Exception
retry :: NFData a =>  Int -> Int -> IO a -> a -> IO a
retry interval count act defVal = do
  result <- try $ act

  case result of
    Left (SomeException e) ->
      if count > 0
      then threadDelay (interval * second_micro_int)
           >> retry interval (count - 1) act defVal
      else return defVal

    -- Evaluate ret to Normal Form to reveal all exceptions within ret
    Right ret -> handle (\(SomeException e) -> return defVal) (ret `deepseq` return ret)


-- Test cases1
utilTest :: Test
utilTest = TestList [TestLabel "Retry test" (TestCase retryAssert)]
  where retryAssert :: Assertion
        retryAssert = do

          t <- retry 1 1 (return $ 5 `div` 0) (1 :: Int)
          assertEqual "Retry" 1 t

          manager <- newManager defaultManagerSettings
          t1 <- retry 1 1 (put_req "http://10.10.10.10" manager) 1
          assertEqual "Retry" 1 t1
