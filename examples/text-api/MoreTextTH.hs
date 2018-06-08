{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
#ifndef ITERATIONS
#define ITERATIONS 1000
#endif
module MoreTextTH where

import Test.Inspection

import qualified Data.Text as T (Text)
import qualified Data.Text.Lazy as LT (Text)
import TextFuns
import Data.List
import Control.Applicative
import Data.Traversable
import Control.Monad
import ShouldDoes
import Test.QuickCheck.Gen

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

randomPipeline :: Gen [Name]
randomPipeline = do
    p <-                 elements [ p | (p,_,_) <- producers ]
    n <- elements [0,1,2]
    ts <- replicateM n $ elements [ t | (t,_,_) <- transformers ]
    c <-                 elements [ c | (c,_,_) <- consumers ]
    return $ [c] ++ ts ++ [p]

randomPipelines :: Int -> IO [[Name]]
randomPipelines n = generate $ vectorOf n randomPipeline


tests :: [(Name, [[Name]], Should, Does)]
tests =
    [ (p, rep consumers <:> rep transformers <:> pure p <:> pure [], s, d)
    | (p,s,d) <- producers ] ++
    [ (t, rep consumers <:> pure t <:> rep producers <:> pure [], s, d)
    | (t,s,d) <- transformers ] ++
    [ (c, pure c <:> rep transformers <:> rep producers <:> pure [], s, d)
    | (c,s,d) <- consumers ]
  where
    rep xs = take 2 [ n | (n,_,Does) <- xs]

(<:>) :: Applicative m => m a -> m [a] -> m [a]
(<:>) = liftA2 (:)
infixr <:>

upTo :: Int -> [a] -> [[a]]
upTo n x = concat [ replicateM m x | m <- [0..n] ]

funName :: [Name] -> String
funName = intercalate "_" . map nameBase


comp :: [ExpQ] -> ExpQ -> ExpQ
comp []     x = x
comp (f:fs) x = appE f (comp fs x)

definePipelines :: DecsQ
definePipelines = do
    random_pipelines <- runIO $ randomPipelines ITERATIONS
    let pipelines = function_pipelines ++ random_pipelines
    putQ random_pipelines
    forM (nub pipelines) $ \pipeline -> do
        x <- newName "x"
        funD (mkName (funName pipeline))
            [clause [varP x] (normalB (comp (map varE pipeline) (varE x))) []]
  where
    function_pipelines = nub $ concat [ xs | (_,xs,_,_) <- tests ]

defineTests :: DecsQ
defineTests = do
    Just other_pipelines <- getQ
    pure <$> funD (mkName "tests") [clause []
        (normalB (listE (tests' ++ otherTests other_pipelines))) []]
  where
    otherTests = map $ \pipeline -> do
        let name = stringE $ intercalate "." (map nameBase pipeline)
        tupE [ name
             , listE [ tupE [ name, mkTest pipeline ] ]
             , lift $ shouldIt pipeline
             , lift $ willIt pipeline
             ]

    mkTest pipeline = do
        -- We need to go through lookupValueName to get a
        -- resolved name that inspection-testing can handle
        Just n <- lookupValueName (funName pipeline)
        inspectTest (mkObligation n (NoTypes [''T.Text, ''LT.Text]))

    tests' = flip map tests $ \(f,pipelines,s,d) -> do
        tupE [ stringE (nameBase f)
             , listE $ flip map pipelines $ \pipeline -> tupE
                  [ stringE $ intercalate "." (map nameBase pipeline)
                  , mkTest pipeline
                  ]
             , lift s
             , lift d
             ]

