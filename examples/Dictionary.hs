{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin Test.Inspection.Plugin #-}
module Dictionary (main) where

import Test.Inspection
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad (replicateM_)
import Data.Traversable (foldMapDefault)
import Data.Semigroup (Semigroup)

putStrLn' :: MonadIO m => String -> m ()
putStrLn' = liftIO . putStrLn

action :: MonadIO m => m ()
action = putStrLn' "foo" >> putStrLn' "bar"

specialized :: IO ()
specialized = action

inspect $ hasNoTypeClasses 'specialized
inspect $ (hasNoTypeClasses 'action) { expectFail = True }

inspect $ hasNoTypeClassesExcept 'action [''MonadIO, ''Monad, ''Applicative, ''Functor]

listFoldMap :: Monoid m => (a -> m) -> [a] -> m
listFoldMap = foldMapDefault

#if __GLASGOW_HASKELL__ >= 802
inspect $ hasNoTypeClassesExcept 'listFoldMap [''Monoid, ''Semigroup]
#else
inspect $ (hasNoTypeClassesExcept 'listFoldMap [''Monoid, ''Semigroup]) { expectFail = True }
#endif


main :: IO ()
main = return ()
