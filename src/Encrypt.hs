-- file: Encrypt.hs

{-# LANGUAGE OverloadedStrings #-}

module Encrypt where

import Test.HUnit
import Crypto.Cipher.RC4

import Data.ByteString.Internal
import Data.String.Conversions

type Key = ByteString
type PlainText = ByteString
type CipherText = ByteString

type EncryptHandle = (Key -> PlainText -> CipherText)
type DecryptHandle = (Key -> CipherText -> PlainText)

data EncryptMethod
  = RC4

encrypt :: EncryptMethod -> Key -> PlainText -> CipherText
encrypt m k plain = encryptMethod m k plain

decrypt :: EncryptMethod -> Key -> CipherText -> PlainText
decrypt m k cipher = decryptMethod m k cipher


encryptMethod :: EncryptMethod -> EncryptHandle
encryptMethod RC4 = encryptWithRC4

decryptMethod :: EncryptMethod -> DecryptHandle
decryptMethod RC4 = decryptWithRC4

encryptWithRC4 :: EncryptHandle
encryptWithRC4 k p =
  let state = initialize k
  in snd $ combine state p

decryptWithRC4 :: DecryptHandle
decryptWithRC4 k c =
  let state = initialize k
  in snd $ combine state c


-- Encrypt Unit Testing
encryptTest :: Test
encryptTest = TestList [TestLabel "Encrypt Unit Testing" (TestCase encryptAssert)]
  where encryptAssert :: Assertion
        encryptAssert = do
          let plainText = "1234567890"
              key = "ajdskfljsdlkfjl;4"
              cipherText = encrypt RC4 key plainText
              plainText' = decrypt RC4 key cipherText

          print (cs plainText' :: String)
