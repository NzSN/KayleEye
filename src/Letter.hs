-- file: Letter.hs

{-# LANGUAGE OverloadedStrings #-}

module Letter where

import KayleConst

import Data.Bool
import Data.Maybe
import Data.Aeson
import Data.Map as Map
import Data.Text.Internal
import Data.ByteString.Lazy hiding (putStrLn)

-- Identity Processing functions
type IdentStr = String
data Identity = Identity { ident_name :: String, ident_sha :: String, ident_event :: String }

emptyIdent :: Identity
emptyIdent = Identity "" "" ""

ident2Str :: Identity -> IdentStr
ident2Str i = (ident_name i) ++ ":" ++ (ident_sha i) ++ ":" ++ (ident_event i)

str2Ident :: IdentStr -> Identity
str2Ident str = Identity (identPart !! 0) (identPart !! 1) (identPart !! 2)
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

-- Control Letter

-- While Doorkeeper of KayleHome receive an req letter
-- it will send a register letter to notify KayleHome core
registerLetter :: String -- Ident
               -> String -- SubTest
               -> Letter
registerLetter ident_ subTest =
  Letter ident_ (fromList [("event", register_event)]) (fromList [("who", subTest)])

-- While Kayle is disconnected accidently DoorKeeper will unregister
-- the correspond subtest
unregisterLetter :: String -- Ident
                 -> String -- SubTest
                 -> Letter
unregisterLetter ident_ subTest =
  Letter ident_ (fromList [("event", unRegister_event)]) (fromList [("who", subTest)])

-- While DoorKeeper of KayleHome receive an disconn letter
-- it will send a register letter to notify KayleHome core
terminatedLetter :: String -- Ident
                 -> String -- Name of sub test
                 -> Letter
terminatedLetter ident_ subTest =
  Letter ident_
  (fromList [("event", terminated_event)])
  (fromList [("who", subTest)])

-- Answer letter is used by KayleHome to answer to DoorKeeper
answerLetter :: IdentStr
             -> String -- Answer
             -> Letter
answerLetter ident_ answer = Letter ident_
                              (fromList [("event", answer_event)])
                              (fromList [(content_answer, answer)])

answerAccepted_Letter :: IdentStr -> Letter
answerAccepted_Letter ident_ = answerLetter ident_ answer_accept

answerRejected_Letter :: IdentStr -> Letter
answerRejected_Letter ident_ = answerLetter ident_ answer_reject

isAnswerOK :: Letter -> Bool
isAnswerOK l =
  let answer = retriFromContent l answer_event
  in maybe False (\an -> bool False True (an == answer_accept)) answer

-- Request letter is used by Kayle to send request to KayleHome
-- to acquire a seqID
reqLetter :: IdentStr
          -> String -- SubTest
          -> Letter
reqLetter ident subTest = Letter ident
                          (fromList [("event", "req")])
                          (fromList [("who", subTest)])

-- Ack letter is used by KayleHome to notify whether the request
-- has beened accepted.
ackLetter :: IdentStr
          -> Bool -- Ture: Accept || False: Rejected
          -> Letter
ackLetter ident i = Letter ident
                    (fromList [("event", ack_event)])
                    (fromList [("answer", answerStr)])
  where answerStr = bool "Rejected" "Accepted" i

ackAcceptLetter ident_ = ackLetter ident_ True
ackRejectedLetter ident_ = ackLetter ident_ False

-- Onece Kayle is done its job it will disconnect from KayleHome via
-- sending a disconn letter
disconnLetter :: IdentStr
              -> String -- SubTest
              -> Letter
disconnLetter ident subTest = Letter ident
                        (fromList [("event", disconn_event)])
                        (fromList [("who", subTest)])



letterBuild :: Letter -> ByteString
letterBuild l = encode $ Letter (ident l) (header l) (content l)

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

typeOfLetter' :: Letter -> String
typeOfLetter' l =
  let identity = str2Ident (ident l)
  in ident_event identity

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
