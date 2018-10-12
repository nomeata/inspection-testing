{-# LANGUAGE TemplateHaskell #-}
module Text (main) where

import Test.Inspection

import Data.Text as T
import Data.Text.Encoding as E
import Data.ByteString (ByteString)

-- Some cases of successful fusion:
toUpperString :: String -> String
toUpperString = T.unpack . T.toUpper . T.pack

toUpperBytestring :: ByteString -> String
toUpperBytestring = T.unpack . T.toUpper . E.decodeUtf8

-- This is the example from the text documentation.
-- Unfortunately it fails, the problem seems to be T.length.
countChars :: ByteString -> Int
countChars = T.length . T.toUpper . E.decodeUtf8

inspect $ 'toUpperString `hasNoType` ''T.Text
inspect $ 'toUpperBytestring `hasNoType` ''T.Text
inspect $ ('countChars `hasNoType` ''T.Text) { expectFail = True }

main :: IO ()
main = return ()
