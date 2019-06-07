{-# LANGUAGE TemplateHaskell #-}

-- This is a kitchen sink test file for various regressions
-- reported via GitHub

import Test.Inspection


-- https://github.com/nomeata/inspection-testing/issues/35
empty1, empty2 :: [a]
empty1 = map id []
empty2 = []

inspect $ 'empty1 === 'empty2

main :: IO ()
main = return ()
