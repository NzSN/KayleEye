{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- file: Logger.hs

module Logger where

import Test.HUnit

import System.IO
import Data.Monoid
import Control.Monad.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Writer


newtype Lbuffer = Lb { runLb :: (Int, String)} deriving (Show)
instance Semigroup Lbuffer
instance Monoid Lbuffer where
  mempty = Lb (0, "")
  mappend l r = Lb $ do
    let lb = runLb l
        rb = runLb r
    ((fst lb) + (fst rb), (snd lb) ++ (snd rb))

bufferLen :: Lbuffer -> Int
bufferLen l = fst . runLb $ l

bufferStr :: Lbuffer -> String
bufferStr l = snd . runLb $ l

isBufferEmpty :: Lbuffer -> Bool
isBufferEmpty l = (bufferLen l) == 0

newtype LoggerT m a = LoggerT { runLoggerT :: m (Writer Lbuffer a) }

instance (Functor m) => Functor (LoggerT m) where
  fmap f = LoggerT . (fmap (fmap f)) . runLoggerT

instance (Functor m, Monad m, Applicative m) => Applicative (LoggerT m) where
  pure = LoggerT . return . return
  lf <*> lx = LoggerT $ do
    f <- runLoggerT lf
    x <- runLoggerT lx
    return $ f <*> x

instance (Functor m, Applicative m, Monad m) => Monad (LoggerT m) where
  return = LoggerT . return . return
  -- Bind
  mx >>= f =  LoggerT $ do
    x <- runLoggerT mx

    let p = runWriter x
        my = f (fst p)

    x1 <- runLoggerT my

    return $ writer (fst $ runWriter x1, (snd p) `mappend` (snd $ runWriter x1))

  x >> y = LoggerT $ do
    x_w <- runLoggerT x
    y_w <- runLoggerT y
    return $ x_w >> y_w

instance MonadTrans LoggerT where
  lift = LoggerT . liftM (\x -> writer (x, mempty))
instance (MonadIO m) => MonadIO (LoggerT m) where
  liftIO = lift . liftIO
instance MonadWriter w m => MonadWriter w (LoggerT m) where
  tell = lift . tell

seperator :: String
seperator = " : "

type Logger a = LoggerT IO a

doLogger :: Logger Integer -> String -> IO Integer
doLogger l path = do
  w <- runLoggerT $ l
  let (code, logMsgs) = runWriter w
  if isBufferEmpty logMsgs
    then return code
    else do file <- openFile path AppendMode
            hPutStrLn file $ bufferStr logMsgs
            hClose file

            return code

appendLogger :: String -- Header
             -> String -- Content
             -> Logger Integer
appendLogger h c = LoggerT $ do
  return $ writer (0, Lb (length h + length c, h ++ ":" ++ c ++ "\n"))

-- Test cases
loggerTest :: Test
loggerTest = TestList [TestLabel "Logger Testing" (TestCase loggerAssert)]
  where loggerAssert :: Assertion
        loggerAssert = do
          let l = do
                appendLogger "123" "456"
                appendLogger "789" "012"
          doLogger l ".\\log.txt"

          return ()
