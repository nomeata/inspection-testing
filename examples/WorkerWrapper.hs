{-# LANGUAGE TemplateHaskell #-}
module WorkerWrapper where

import Test.Inspection

-- In this module, we are interested in checking if the worker-wrapper transformation is firing.
-- That transformation is described here:
--    https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/compiler/demand#worker-wrapper-split
-- In short, we don't want to be passing around the dictionary type below, but rather have GHC
-- generate a helper function @$wdictFold :: (m -> m -> m) -> m -> [m] -> m@, as this saves us some
-- wrapping and unwrapping work at each iteration.
--
-- GHC still generates a wrapper at the @dictFold@ name. That is, a function that still consumes the
-- @MonoidDict@ type, but unwraps it just once, before passing it off to @wdictFold@ to handle the
-- recuresion. As @dictFold@ refers to this wrapper, we cannot check that the @MonoidDict@ type is
-- unused in its definition.
--
-- So what are we to do? As the generated wrapper should have an @INLINE@ pragma, once we pass it in
-- the @MonoidDict@, we should be safely in worker-land, and not need to reference @MonoidDict@
-- again. Thus, we look at @appliedFold@, and assert that @MonoidDict@ never shows up -- as would be
-- expected if worker-wrapper fired as hoped.
data MonoidDict a = MonoidDict { dictMappend :: a -> a -> a, dictMempty :: a }

dictFold :: MonoidDict m -> [m] -> m
dictFold bm xs = case xs of
  [] -> dictMempty bm
  x:xs' -> dictMappend bm x $ dictFold bm xs'

appliedFold :: [Int] -> Int
appliedFold = dictFold $ MonoidDict (+) 0

inspect $ 'appliedFold `doesNotUse` 'MonoidDict

main :: IO ()
main = return ()
