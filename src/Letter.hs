-- file: Letter.hs

{-# LANGUAGE OverloadedStrings #-}

module Letter where

import Data.Maybe
import Data.Aeson
import Data.Map as Map
import Data.Text.Internal

-- Identity Processing functions
type IdentStr = String
data Identity = Identity { ident_name :: String, ident_sha :: String, ident_event :: String }

emptyIdent :: Identity
emptyIdent = Identity "" "" ""

ident2Str :: Identity -> IdentStr
ident2Str i = (ident_name i) ++ ":" ++ (ident_sha i) ++ ":" ++ (ident_event i)

str2Ident :: IdentStr -> Identity
str2Ident str = Identity (identPart !! 1) (identPart !! 2) (identPart !! 3)
  where identPart = identSplit str

identSplit :: String -> [String]
identSplit [] = [""]
identSplit all@(x:xs)
  | x == ':'   = "" : shaPart
  | otherwise = (x : (Prelude.head $ identSplit xs)) : Prelude.tail shaPart

  where shaPart = identSplit xs

-- Letter
data Letter = Letter { ident :: IdentStr,
                       header :: Map String String,
                       content :: Map String String } | Empty_letter deriving Show

instance ToJSON Letter where
  toJSON (Letter ident header content) =
    object ["ident" .= ident,
            "header" .= header,
            "content" .= content]

instance FromJSON Letter where
  parseJSON = withObject "Letter" $ \v -> Letter
    <$> v .: "ident"
    <*> v .: "header"
    <*> v .: "content"

sizeOfLetter :: Letter -> Int
sizeOfLetter l = size . content $ l

isEmptyLetter :: Letter -> Bool
isEmptyLetter Empty_letter = True
isEmptyLetter _ = False

emptyLetter :: Letter
emptyLetter = Letter "" Map.empty Map.empty

letterUpdate :: Letter
             -> String -- Key
             -> String -- New Value
             -> Letter
letterUpdate l k nv =
  Letter (ident l) (header l) (updateWithKey (\k_ v_ -> Just nv) k (content l))

letterUpdate' :: Letter
              -> [(String, String)]
              -> Letter
letterUpdate' l [] = l
letterUpdate' l (k:ks) = do
  letterUpdate' (letterUpdate l (fst k) (snd k)) ks

allKeysOfContent :: Letter -> [String]
allKeysOfContent l = keys $ content l

retriFromContent :: Letter
                 -> String -- Key
                 -> Maybe String
retriFromContent l k = Map.lookup k (content l)

retriFromHeader :: Letter
                -> String -- key
                -> Maybe String
retriFromHeader l k = Map.lookup k (header l)

typeOfLetter :: Letter -> String
typeOfLetter l =
  let type_ = retriFromHeader l "event"

  in if isNothing type_
     then ""
     else fromJust type_

-- Check letter content to see that is test pass
isTestFinished :: Letter -> Bool
isTestFinished l = let content_of_l = content l
                   in Prelude.foldl (\acc x -> acc && x) True
                      [ (fromJust $ Map.lookup k content_of_l) /= "O" | k <- allKeysOfContent l]

isTestSuccess :: Letter -> Bool
isTestSuccess l = let content_of_l = content l
                  in Prelude.foldl (\acc x -> acc && x) True
                     [ (fromJust $ Map.lookup k content_of_l) == "T" | k <- allKeysOfContent l]
