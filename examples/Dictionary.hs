{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module Dictionary (main) where

import Test.Inspection
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (replicateM_)

putStrLn' :: MonadIO m =>  String -> m ()
putStrLn' = liftIO . putStrLn

action :: MonadIO m => m ()
action = replicateM_ 10 (putStrLn' "foo")

specialized :: IO ()
specialized = action

inspect $ hasNoDicts 'specialized
inspect $ (hasNoDicts 'action) { expectFail = True }

inspect $ hasNoDictsExcept 'action [''MonadIO, ''Monad, ''Applicative, ''Functor]

main :: IO ()
main = return ()
