{-# LANGUAGE TemplateHaskell #-}
module DoesNotUse where

import Test.Inspection
import Control.Exception.Base (patError)

matches (Just _) = True
matches Nothing = False

partial (Just _) = Nothing


inspect $ ('matches `doesNotUse` 'Just) { expectFail = True }
inspect $ 'matches `doesNotUse` 'patError
inspect $ ('partial `doesNotUse` 'patError) { expectFail = True }
inspect $ ('partial `doesNotUse` 'Nothing) { expectFail = True }

main :: IO ()
main = return ()
