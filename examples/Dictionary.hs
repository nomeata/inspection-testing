{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module Dictionary (main) where

import Test.Inspection
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (replicateM_)
import Data.Traversable (foldMapDefault)
import Data.Semigroup (Semigroup)

putStrLn' :: MonadIO m =>  String -> m ()
putStrLn' = liftIO . putStrLn

action :: MonadIO m => m ()
action = replicateM_ 10 (putStrLn' "foo")

specialized :: IO ()
specialized = action

inspect $ hasNoDicts 'specialized
inspect $ (hasNoDicts 'action) { expectFail = True }

inspect $ hasNoDictsExcept 'action [''MonadIO, ''Monad, ''Applicative, ''Functor]

listFoldMap :: Monoid m => (a -> m) -> [a] -> m
listFoldMap = foldMapDefault

#if __GLASGOW_HASKELL__ >= 802
inspect $ hasNoDictsExcept 'listFoldMap [''Monoid, ''Semigroup]
#else
inspect $ (hasNoDictsExcept 'listFoldMap [''Monoid, ''Semigroup]) { expectFail = True }
#endif


main :: IO ()
main = return ()
