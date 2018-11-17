{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}
module LazyTextFuns
    ( producers
    , consumers
    , transformers
    , untestable
    , allFuns
    , shouldIt
    , willIt
    ) where

import qualified Prelude as P
import Prelude (Maybe(..), succ, fromEnum, toEnum, (+), ($), (++),
                Monoid(..), Semigroup(..), Eq(..), Int)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as E
import Data.Char (isAscii)
import Data.Int

import ShouldDoes

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

untestable :: [Name]
untestable =
  [ 'T.uncons -- supposed to fuse
  , 'T.fromChunks
  , 'T.toChunks
  , 'T.toStrict
  , 'T.fromStrict
  , 'T.foldrChunks
  , 'T.foldlChunks
  , 'T.intercalate
  , 'T.replace
  , 'T.concat
  , 'T.concatMap
  , 'T.mapAccumL
  , 'T.mapAccumR
  , 'T.splitAt
  , 'T.breakOn
  , 'T.breakOnEnd
  , 'T.break
  , 'T.span
  , 'T.group
  , 'T.groupBy
  , 'T.inits
  , 'T.tails
  , 'T.splitOn
  , 'T.split
  , 'T.chunksOf
  , 'T.lines
  , 'T.words
  , 'T.unlines
  , 'T.unwords
  , 'T.stripPrefix
  , 'T.stripSuffix
  , 'T.commonPrefixes
  , 'T.breakOnAll
  , 'T.partition
  , 'T.count
  , 'T.zip
  , 'T.zipWith
  ]


i :: Int64
i = 42
{-# NOINLINE i #-}

empty         _ = T.empty
{-# INLINE empty #-}
take          x = T.take i x
{-# INLINE take #-}
takeEnd       x = T.takeEnd i x
{-# INLINE takeEnd #-}
drop          x = T.drop i x
{-# INLINE drop #-}
dropEnd       x = T.dropEnd i x
{-# INLINE dropEnd #-}
cons          x = 'x' `T.cons` x
{-# INLINE cons #-}
snoc          x = x `T.snoc` 'x'
{-# INLINE snoc #-}
map           x = T.map succ x
{-# INLINE map #-}
justifyLeft   x = T.justifyLeft 42 'x' x
{-# INLINE justifyLeft #-}
justifyRight  x = T.justifyRight 42 'x' x
{-# INLINE justifyRight #-}
center        x = T.center i 'x' x
{-# INLINE center #-}
intersperse   x = T.intersperse 'x' x
{-# INLINE intersperse #-}
append        x = unfoldrN 'y' `T.append` x
{-# INLINE append #-}
isPrefixOf    x = unfoldrN 'a' `T.isPrefixOf` x
{-# INLINE isPrefixOf #-}
isSuffixOf    x = unfoldrN 'b' `T.isSuffixOf` x
{-# INLINE isSuffixOf #-}
isInfixOf     x = unfoldrN 'c' `T.isInfixOf` x
{-# INLINE isInfixOf #-}
compareLength x = x `T.compareLength` i
{-# INLINE compareLength #-}
foldl         x = T.foldl   (\x c -> x + fromEnum c) 0 x
{-# INLINE foldl #-}
foldl'        x = T.foldl'  (\x c -> x + fromEnum c) 0 x
{-# INLINE foldl' #-}
foldl1        x = T.foldl1  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) x
{-# INLINE foldl1 #-}
foldl1'       x = T.foldl1' (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) x
{-# INLINE foldl1' #-}
foldr         x = T.foldr   (\c x -> x + fromEnum c) 0 x
{-# INLINE foldr #-}
foldr1        x = T.foldr1  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) x
{-# INLINE foldr1 #-}
any           x = T.any isAscii x
{-# INLINE any #-}
all           x = T.all isAscii x
{-# INLINE all #-}
scanl         x = T.scanl  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) 'x' x
{-# INLINE scanl #-}
scanl1        x = T.scanl1 (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2)     x
{-# INLINE scanl1 #-}
scanr         x = T.scanr  (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2) 'x' x
{-# INLINE scanr #-}
scanr1        x = T.scanr1 (\c1 c2 -> toEnum $ fromEnum c1 + fromEnum c2)     x
{-# INLINE scanr1 #-}
unfoldr       x = T.unfoldr    (\c -> if c  == 'z' then Nothing else Just (c, succ c)) x
{-# INLINE unfoldr #-}
unfoldrN      x = T.unfoldrN i (\c -> if c  == 'z' then Nothing else Just (c, succ c)) x
{-# INLINE unfoldrN #-}
takeWhile     x = T.takeWhile isAscii x
{-# INLINE takeWhile #-}
dropWhile     x = T.dropWhile isAscii x
{-# INLINE dropWhile #-}
takeWhileEnd  x = T.takeWhileEnd isAscii x
{-# INLINE takeWhileEnd #-}
dropWhileEnd  x = T.dropWhileEnd isAscii x
{-# INLINE dropWhileEnd #-}
dropAround    x = T.dropAround isAscii x
{-# INLINE dropAround #-}
filter        x = T.filter isAscii x
{-# INLINE filter #-}
find          x = T.find isAscii x
{-# INLINE find #-}
replicate     x = T.replicate i x
{-# INLINE replicate #-}
index         x = x `T.index` i
{-# INLINE index #-}
iterate       x = T.iterate succ x
{-# INLINE iterate #-}

producers, transformers, consumers :: [(Name, Should, Does)]
producers =
    [ s_d_ 'T.pack
    , sndn 'E.decodeUtf8
    , sndn 'empty
    , s_d_ 'unfoldr
    , s_d_ 'unfoldrN
    , sndn 'T.repeat
    , sndn 'iterate
    , s_d_ 'T.singleton
    ]
transformers =
    [ s_d_ 'cons
    , s_d_ 'snoc
    , s_d_ 'append
    , s_d_ 'T.tail
    , s_d_ 'T.init
    , s_d_ 'map
    , s_d_ 'intersperse
    , sndn 'T.reverse
    , s_d_ 'T.toCaseFold
    , s_d_ 'T.toLower
    , s_d_ 'T.toUpper
    , s_d_ 'T.toTitle
    , s_d_ 'justifyLeft
    , sndn 'justifyRight
    , sndn 'center
    , s_d_ 'scanl
    , sndn 'scanl1
    , sndn 'scanr
    , sndn 'scanr1
    , sndn 'replicate
    , sndn 'T.cycle
    , s_d_ 'take
    , sndn 'takeEnd
    , s_d_ 'drop
    , sndn 'dropEnd
    , s_d_ 'takeWhile
    , sndn 'takeWhileEnd
    , s_d_ 'dropWhile
    , sndn 'dropWhileEnd
    , sndn 'dropAround
    , sndn 'T.strip
    , s_d_ 'T.stripStart
    , sndn 'T.stripEnd
    , s_d_ 'filter
    ]
consumers =
    [ s_d_ 'T.unpack
    , s_d_ 'T.head
    , s_d_ 'T.last
    , s_d_ 'T.null
    , s_d_ 'T.length
    , s_d_ 'compareLength
    , s_d_ 'foldl
    , s_d_ 'foldl'
    , s_d_ 'foldl1
    , s_d_ 'foldl1'
    , s_d_ 'foldr
    , s_d_ 'foldr1
    , s_d_ 'any
    , s_d_ 'all
    , s_d_ 'T.maximum
    , s_d_ 'T.minimum
    , s_d_ 'find
    , s_d_ 'index
    -- , s_d_ 'findIndex
    , s_d_ 'isPrefixOf
    , sndn 'isSuffixOf
    , sndn 'isInfixOf
    ]

allFuns = producers ++ transformers ++ consumers

shouldIt :: [Name] -> Should
shouldIt = P.foldMap $ \n -> mconcat [ s | (n',s,_) <- allFuns, n == n' ]

willIt  :: [Name] -> Does
willIt pipeline
    -- No idea about singleton (makes stuff work, like `center`, and breaks others)
    | 'T.singleton `P.elem` pipeline       = May
    -- Are all good functions?
    | P.all (`P.elem` good_funs) pipeline  = Does
    -- Some special cases
    | pipeline == ['T.last, 'empty]        = Does
    | pipeline == ['T.null, 'empty]        = Does
    -- Otherwise, probably not
    | P.otherwise                          = DoesNot
  where
    good_funs = [ n | (n, _, Does) <- allFuns ]

