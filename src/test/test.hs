-- file: test.hs

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import Data.Aeson.Types

import Data.Maybe

json_main = do
  x <- (decode $ encode $ object [ "a" .= object [ "b" .= ("c" :: String) ], "d" .= ("e" :: String)] :: Maybe Object)
  flip parseMaybe x $ \obj -> do
    v0 <- obj .: "a"
    return $ (v0 :: Object)

json_main1 = do
  result <- decode "{\"name\":\"Dave\",\"age\":2}"
  flip parseMaybe result $ \obj -> do
    age <- obj .: "age"
    name <- obj .: "name"
    return $ name ++ ":" ++ show ((age :: Int) * 2)

main = print $ json_main
