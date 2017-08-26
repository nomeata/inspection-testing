{-# LANGUAGE BangPatterns, ScopedTypeVariables, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -O -fplugin GHC.Proof.Plugin #-}

-- This modules explores which rules from hlint-1.9.41 we can prove with
-- the GHC.Proof plugin.
--
-- Those with === are proved. Those with =/= not yet.
module HLint where

import GHC.Proof

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.Foldable
import Data.Foldable(asum, sequenceA_, traverse_, for_)
import Data.Traversable(traverse, for)
import Control.Applicative
import Data.Function
import Data.Int
import Data.Char
import Data.List as Data.List
import Data.List as X
import Data.Maybe
import Data.Monoid
import System.IO
import Control.Concurrent.Chan
import System.Mem.Weak
import Control.Exception.Base
import System.Exit
import Data.Either
import Numeric

-- I/O

-- warn = putStrLn (show x) ==> print x
proof1 x = putStrLn (show x) === print x

-- warn = mapM_ putChar ==> putStr
proof2 = mapM_ putChar =/= putStr

-- warn = hGetChar stdin ==> getChar
proof3 = hGetChar stdin === getChar

-- warn = hGetLine stdin ==> getLine
proof4 = hGetLine stdin === getLine

-- warn = hGetContents stdin ==> getContents
proof5 = hGetContents stdin === getContents

-- warn = hPutChar stdout ==> putChar
proof6 = hPutChar stdout =/= putChar

-- warn = hPutStr stdout ==> putStr
proof7 = hPutStr stdout =/= putStr

-- warn = hPutStrLn stdout ==> putStrLn
proof8 = hPutStrLn stdout =/= putStrLn

-- warn = hPrint stdout ==> print
proof9 :: forall x. Show x => Proof
proof9 = (hPrint stdout :: x -> IO ()) === print

-- warn = hWaitForInput a 0 ==> hReady a
proof10 a = hWaitForInput a 0 === hReady a

-- warn = hPutStrLn a (show b) ==> hPrint a b
proof11 a b = hPutStrLn a (show b) === hPrint a b

-- warn = hIsEOF stdin ==> isEOF
proof12 = hIsEOF stdin === isEOF

-- -- EXIT

-- warn = exitWith ExitSuccess ==> exitSuccess
proof13 = exitWith ExitSuccess === exitSuccess

-- -- ORD

-- warn = not (a == b) ==> a /= b where note = "incorrect if either value is NaN"
proof14a a (b::Bool) = not (a == b) === (a /= b)
proof14b a (b::Int)  = not (a == b) =/= (a /= b)
proof14c a b         = not (a == b) =/= (a /= b)

-- warn = not (a /= b) ==> a == b where note = "incorrect if either value is NaN"
proof15 a (b::Bool) = not (a /= b) === (a == b)

-- warn = not (a >  b) ==> a <= b where note = "incorrect if either value is NaN"
proof16 a (b::Bool) = not (a > b) === (a <= b)

-- warn = not (a >= b) ==> a <  b where note = "incorrect if either value is NaN"
proof17 a (b::Bool) = not (a >= b) === (a < b)

-- warn = not (a <  b) ==> a >= b where note = "incorrect if either value is NaN"
proof18 a (b::Bool) = not (a < b) === (a >= b)

-- warn = not (a <= b) ==> a >  b where note = "incorrect if either value is NaN"
proof19 a (b::Bool) = not (a <= b) === (a > b)

-- warn = compare x y /= GT ==> x <= y
-- warn = compare x y == LT ==> x < y
-- warn = compare x y /= LT ==> x >= y
-- warn = compare x y == GT ==> x > y
-- warn = compare x y == EQ ==> x == y
-- warn = compare x y /= EQ ==> x /= y
-- --warning = x == a || x == b || x == c ==> x `elem` [a,b,c] where note = ValidInstance "Eq" x
-- --warning = x /= a && x /= b && x /= c ==> x `notElem` [a,b,c] where note = ValidInstance "Eq" x
-- --warn = compare (f x) (f y) ==> Data.Ord.comparing f x y -- not that great
-- --warn = on compare f ==> Data.Ord.comparing f -- not that great
-- warn = head (sort x) ==> minimum x
-- warn = last (sort x) ==> maximum x
-- warn = head (sortBy f x) ==> minimumBy f x
--     where _ = isCompare f
-- warn = last (sortBy f x) ==> maximumBy f x
--     where _ = isCompare f
-- warn "Avoid reverse" = reverse (sort x) ==> sortBy (flip compare) x
-- warn "Avoid reverse" = reverse (sortBy f x) ==> sortBy (flip f) x
--     where _ = isCompare f
-- hint = flip (g `on` h) ==> flip g `on` h
proof20 g h = flip (g `on` h) === (flip g `on` h)


-- hint = (f `on` g) `on` h ==> f `on` (g . h)
proof21 f g h = ((f `on` g) `on` h) === (f `on` (g.h))



-- -- READ/SHOW

-- warn = showsPrec 0 x "" ==> show x
-- warn = readsPrec 0 ==> reads
-- warn = showsPrec 0 ==> shows
-- hint = showIntAtBase 16 intToDigit ==> showHex
-- hint = showIntAtBase 8 intToDigit ==> showOct

-- -- LIST

-- warn = concat (map f x) ==> concatMap f x
proof22 f x = concat (map f x) =/= concatMap f x

-- warn = concat (fmap f x) ==> concatMap f x
proof23 :: forall f. (Functor f, Foldable f) => Proof
proof23 = (\ f x -> concat (fmap f x)) =/= (\ f (x:: f a) -> concatMap f x)

-- hint = concat [a, b] ==> a ++ b
proof24 a b = concat [a, b] === (a ++ b)

-- hint "Use map once" = map f (map g x) ==> map (f . g) x
proof25 f g x = map f (map g x) === map (f . g) x

-- hint "Fuse concatMap/map" = concatMap f (map g x) ==> concatMap (f . g) x
proof26 f g x = concatMap f (map g x) =/= concatMap (f . g) x

-- hint = x !! 0 ==> head x
proof27 x = x !! 0 =/= head x

-- warn = take n (repeat x) ==> replicate n x
--     where _ = noQuickCheck -- takes too long
proof28 n x = take n (repeat x) === replicate n x

-- warn = map f (replicate n x) ==> replicate n (f x)
--     where _ = noQuickCheck -- takes too long
proof29 f n x = map f (replicate n x) =/= replicate n (f x)

-- warn = map f (repeat x) ==> repeat (f x)
--     where _ = noQuickCheck -- takes forever
proof30 f x = map f (repeat x) =/= repeat (f x)
  -- ^ works if we simplify all the way to phase 0!

-- warn = cycle [x] ==> repeat x
--     where _ = noQuickCheck -- takes forever
proof31 x = cycle [x] =/= repeat x

-- warn = head (reverse x) ==> last x
-- warn = head (drop n x) ==> x !! n where _ = isNat n
-- warn = reverse (tail (reverse x)) ==> init x where note = IncreasesLaziness
-- warn "Avoid reverse" = reverse (reverse x) ==> x where note = IncreasesLaziness
-- -- warn = take (length x - 1) x ==> init x -- not true for x == []
-- warn = isPrefixOf (reverse x) (reverse y) ==> isSuffixOf x y

-- warn = foldr (++) [] ==> concat
proof32 x = foldr (++) [] x === concat x

-- warn = foldr (++) "" ==> concat
proof33 x = foldr (++) "" x === concat x

-- warn = foldl (++) [] ==> concat where note = IncreasesLaziness
-- warn = foldl (++) "" ==> concat where note = IncreasesLaziness
-- warn = foldl f (head x) (tail x) ==> foldl1 f x
-- warn = foldr f (last x) (init x) ==> foldr1 f x
-- warn = span (not . p) ==> break p
proof34 p = span (not . p) =/= break p

-- warn = break (not . p) ==> span p
proof35 p = break (not . p) =/= span p

-- warn = (takeWhile p x, dropWhile p x) ==> span p x
proof36 p x = (takeWhile p x, dropWhile p x) =/= span p x

-- warn = fst (span p x) ==> takeWhile p x
proof37 p x = fst (span p x) =/= takeWhile p x

-- warn = snd (span p x) ==> dropWhile p x
proof38 p x = snd (span p x) =/= dropWhile p x

-- warn = fst (break p x) ==> takeWhile (not . p) x
proof39 p x = fst (break p x) =/= takeWhile (not . p) x

-- warn = snd (break p x) ==> dropWhile (not . p) x
proof40 p x = snd (break p x) =/= dropWhile (not . p) x

-- warn = concatMap (++ "\n") ==> unlines
proof41 = concatMap (++ "\n") =/= unlines

-- warn = map id ==> id
proof42 = map id =/= id

-- warn = concatMap id ==> concat
proof43 x = concatMap id x === concat x

-- warn = or (map p x) ==> any p x
proof44 p x = or (map p x) =/= any p x

-- warn = and (map p x) ==> all p x
proof45 p x = and (map p x) =/= all p x

-- warn = zipWith (,) ==> zip
proof46 = zipWith (,) =/= zip

-- warn = zipWith3 (,,) ==> zip3
proof47 = zipWith3 (,,) =/= zip3

-- hint = length x == 0 ==> null x where note = IncreasesLaziness
-- hint = x == [] ==> null x
proof48 x = (x `seq` x == []) =/= null x

-- hint "Use null" = length x /= 0 ==> not (null x) where note = IncreasesLaziness
-- hint "Use :" = (\x -> [x]) ==> (:[])
proof49 x = (\x -> [x]) === (:[])

-- warn = map (uncurry f) (zip x y) ==> zipWith f x y
proof50 f x y = map (uncurry f) (zip x y) =/= zipWith f x y

-- hint = map f (zip x y) ==> zipWith (curry f) x y where _ = isVar f
proof51 f x y = map f (zip x y) =/= zipWith (curry f) x y

-- warn = not (elem x y) ==> notElem x y
proof52 x y = not (elem x y) === notElem x y

-- hint = foldr f z (map g x) ==> foldr (f . g) z x
proof53 f z g x = foldr f z (map g x) =/= foldr (f . g) z x

-- warn = x ++ concatMap (' ':) y ==> unwords (x:y)
proof54 x y = x ++ concatMap (' ':) y =/= unwords (x:y)

-- warn = intercalate " " ==> unwords
proof55 x = intercalate " " x =/= unwords x

-- hint = concat (intersperse x y) ==> intercalate x y where _ = notEq x " "
proof56 x y = concat (intersperse x y) =/= intercalate x y 

-- hint = concat (intersperse " " x) ==> unwords x
proof57 x = concat (intersperse " " x) =/= unwords x

-- warn "Use any" = null (filter f x) ==> not (any f x)
proof58 f x = null (filter f x) =/= not (any f x)

-- warn "Use any" = filter f x == [] ==> not (any f x)
-- warn = filter f x /= [] ==> any f x
-- warn = any id ==> or
proof60 x = any id x =/= or x
proof60list (x::[Bool]) = any id x === or x

-- warn = all id ==> and
proof61 x = all id x =/= and x
proof61list (x::[Bool]) = all id x === and x

-- warn = any ((==) a) ==> elem a where note = ValidInstance "Eq" a
proof62 a x = any ((==) a) x =/= elem a x
proof62list a (x::[a]) = any ((==) a) x =/= elem a x

-- warn = any (== a) ==> elem a
proof63 a x = any (== a) x =/= elem a x
proof63list a (x::[a]) = any (== a) x =/= elem a x

-- warn = any (a ==) ==> elem a where note = ValidInstance "Eq" a
proof64 a x = any (a ==) x =/= elem a x
proof64list a (x::[a]) = any (a ==) x =/= elem a x

-- warn = all ((/=) a) ==> notElem a where note = ValidInstance "Eq" a
-- warn = all (/= a) ==> notElem a where note = ValidInstance "Eq" a
-- warn = all (a /=) ==> notElem a where note = ValidInstance "Eq" a
-- warn = elem True ==> or
proof65 x = elem True x =/= or x
proof65list (x::[Bool]) = elem True x =/= or x

-- warn = notElem False ==> and
proof66 x = notElem False x =/= and x
proof66list (x::[Bool]) = notElem False x =/= and x

-- warn = findIndex ((==) a) ==> elemIndex a
-- warn = findIndex (a ==) ==> elemIndex a
-- warn = findIndex (== a) ==> elemIndex a
-- warn = findIndices ((==) a) ==> elemIndices a
-- warn = findIndices (a ==) ==> elemIndices a
-- warn = findIndices (== a) ==> elemIndices a
-- warn = lookup b (zip l [0..]) ==> elemIndex b l
proof67 b l = lookup b (zip l [0..]) =/= elemIndex b l

-- hint = elem x [y] ==> x == y where note = ValidInstance "Eq" a
proof68 x y = elem x [y] =/= x == y

-- hint = notElem x [y] ==> x /= y where note = ValidInstance "Eq" a
proof69 x y = notElem x [y] =/= x /= y

-- hint "Length always non-negative" = length x >= 0 ==> True
-- hint "Use null" = length x > 0 ==> not (null x) where note = IncreasesLaziness
-- hint "Use null" = length x >= 1 ==> not (null x) where note = IncreasesLaziness
-- warn "Take on a non-positive" = take i x ==> [] where _ = isNegZero i
-- warn "Drop on a non-positive" = drop i x ==> x where _ = isNegZero i
-- warn = last (scanl f z x) ==> foldl f z x
-- warn = head (scanr f z x) ==> foldr f z x
-- warn = iterate id ==> repeat
--     where _ = noQuickCheck -- takes forever
proof70 x = iterate id x =/= repeat x

-- warn = zipWith f (repeat x) ==> map (f x)
proof71 f x y = zipWith f (repeat x) y =/= map (f x) y

-- warn = zipWith f y (repeat z) ==> map (\x -> f x z) y
proof72 f y z = zipWith f y (repeat z) =/= map (\x -> f x z) y

-- -- BY

-- warn = deleteBy (==) ==> delete
proof73 x xs = deleteBy (==) x xs === delete x xs

-- warn = groupBy (==) ==> group
proof74 xs = groupBy (==) xs === group xs

-- warn = insertBy compare ==> insert
proof75 x xs = insertBy compare x xs === insert x xs

-- warn = intersectBy (==) ==> intersect
proof76 xs = intersectBy (==) xs === intersect xs

-- warn = maximumBy compare ==> maximum
proof77 xs = maximumBy compare xs =/= maximum xs
proof77list (xs::[a]) = maximumBy compare xs =/= maximum xs

-- warn = minimumBy compare ==> minimum
proof78 xs = minimumBy compare xs =/= minimum xs
proof78list (xs::[a]) = minimumBy compare xs =/= minimum xs

-- warn = nubBy (==) ==> nub
proof79 xs = nubBy (==) xs === nub xs

-- warn = sortBy compare ==> sort
proof80 xs = sortBy compare xs === sort xs

-- warn = unionBy (==) ==> union
proof81 xs = unionBy (==) xs === union xs

-- -- FOLDS

-- warn = foldr  (>>) (return ()) ==> sequence_
--     where _ = noQuickCheck
-- warn = foldr  (&&) True ==> and
-- warn = foldl  (&&) True ==> and where note = IncreasesLaziness
-- warn = foldr1 (&&)  ==> and where note = RemovesError "on []"; _ = noQuickCheck
-- warn = foldl1 (&&)  ==> and where note = RemovesError "on []"
-- warn = foldr  (||) False ==> or
-- warn = foldl  (||) False ==> or where note = IncreasesLaziness
-- warn = foldr1 (||)  ==> or where note = RemovesError "on []"
-- warn = foldl1 (||)  ==> or where note = RemovesError "on []"
-- warn = foldl  (+) 0 ==> sum
-- warn = foldr  (+) 0 ==> sum
-- warn = foldl1 (+)   ==> sum where note = RemovesError "on []"
-- warn = foldr1 (+)   ==> sum where note = RemovesError "on []"
-- warn = foldl  (*) 1 ==> product
-- warn = foldr  (*) 1 ==> product
-- warn = foldl1 (*)   ==> product where note = RemovesError "on []"
-- warn = foldr1 (*)   ==> product where note = RemovesError "on []"
-- warn = foldl1 max   ==> maximum
-- warn = foldr1 max   ==> maximum
-- warn = foldl1 min   ==> minimum
-- warn = foldr1 min   ==> minimum
-- warn = foldr mplus mzero ==> msum
--     where _ = noQuickCheck

-- -- FUNCTION

-- warn = (\x -> x) ==> id
-- warn = (\x y -> x) ==> const
-- warn = (\(x,y) -> y) ==> snd
-- warn = (\(x,y) -> x) ==> fst
-- hint "Use curry" = (\x y -> f (x,y)) ==> curry f
-- hint "Use uncurry" = (\(x,y) -> f x y) ==> uncurry f where note = IncreasesLaziness
-- warn "Redundant $" = (($) . f) ==> f
-- warn "Redundant $" = (f $) ==> f
-- hint = (\x -> y) ==> const y where _ = isAtom y && not (isWildcard y)
--     -- isWildcard because some people like to put brackets round them even though they are atomic
-- warn "Redundant flip" = flip f x y ==> f y x where _ = isApp original
-- warn "Evaluate" = id x ==> x
--     where _ = not (isTypeApp x)
-- warn "Redundant id" = id . x ==> x
-- warn "Redundant id" = x . id ==> x

-- -- CHAR

-- warn = a >= 'a' && a <= 'z' ==> isAsciiLower a
-- warn = a >= 'A' && a <= 'Z' ==> isAsciiUpper a
-- warn = a >= '0' && a <= '9' ==> isDigit a
-- warn = a >= '0' && a <= '7' ==> isOctDigit a
-- warn = isLower a || isUpper a ==> isAlpha a
-- warn = isUpper a || isLower a ==> isAlpha a

-- -- BOOL

-- warn "Redundant ==" = x == True ==> x
-- hint "Redundant ==" = x == False ==> not x
-- warn "Redundant ==" = True == a ==> a
-- hint "Redundant ==" = False == a ==> not a
-- warn "Redundant /=" = a /= True ==> not a
-- hint "Redundant /=" = a /= False ==> a
-- warn "Redundant /=" = True /= a ==> not a
-- hint "Redundant /=" = False /= a ==> a
-- warn "Redundant if" = (if a then x else x) ==> x where note = IncreasesLaziness
-- warn "Redundant if" = (if a then True else False) ==> a
-- warn "Redundant if" = (if a then False else True) ==> not a
-- warn "Redundant if" = (if a then t else (if b then t else f)) ==> if a || b then t else f
-- warn "Redundant if" = (if a then (if b then t else f) else f) ==> if a && b then t else f
-- warn "Redundant if" = (if x then True else y) ==> x || y where _ = notEq y False
-- warn "Redundant if" = (if x then y else False) ==> x && y where _ = notEq y True
-- hint "Use if" = case a of {True -> t; False -> f} ==> if a then t else f
-- hint "Use if" = case a of {False -> f; True -> t} ==> if a then t else f
-- hint "Use if" = case a of {True -> t; _ -> f} ==> if a then t else f
-- hint "Use if" = case a of {False -> f; _ -> t} ==> if a then t else f
-- hint "Redundant if" = (if c then (True, x) else (False, x)) ==> (c, x) where note = IncreasesLaziness
-- hint "Redundant if" = (if c then (False, x) else (True, x)) ==> (not c, x) where note = IncreasesLaziness
-- hint = or [x, y] ==> x || y
-- hint = or [x, y, z] ==> x || y || z
-- hint = and [x, y] ==> x && y
-- hint = and [x, y, z] ==> x && y && z
-- warn "Redundant if" = (if x then False else y) ==> not x && y where _ = notEq y True
-- warn "Redundant if" = (if x then y else True) ==> not x || y where _ = notEq y False
-- warn "Redundant not" = not (not x) ==> x
-- -- warn "Too strict if" = (if c then f x else f y) ==> f (if c then x else y) where note = IncreasesLaziness
-- -- also breaks types, see #87

-- -- ARROW

-- warn = id *** g ==> second g
-- warn = f *** id ==> first f
-- warn = zip (map f x) (map g x) ==> map (f Control.Arrow.&&& g) x
-- hint = (\(x,y) -> (f x, g y)) ==> f Control.Arrow.*** g
-- hint = (\x -> (f x, g x)) ==> f Control.Arrow.&&& g
-- hint = (\(x,y) -> (f x,y)) ==> Control.Arrow.first f
-- hint = (\(x,y) -> (x,f y)) ==> Control.Arrow.second f
-- hint = (f (fst x), g (snd x)) ==> (f Control.Arrow.*** g) x
-- hint "Redundant pair" = (fst x, snd x) ==>  x where note = DecreasesLaziness

-- -- FUNCTOR

-- warn "Functor law" = fmap f (fmap g x) ==> fmap (f . g) x where _ = noQuickCheck
-- warn "Functor law" = f <$> g <$> x ==> f . g <$> x where _ = noQuickCheck
-- warn "Functor law" = fmap id ==> id where _ = noQuickCheck
-- warn "Functor law" = id <$> x ==> x where _ = noQuickCheck
-- hint = fmap f $ x ==> f Control.Applicative.<$> x
--     where _ = (isApp x || isAtom x) && noQuickCheck

-- -- MONAD

-- warn "Monad law, left identity" = return a >>= f ==> f a where _ = noQuickCheck
-- warn "Monad law, left identity" = f =<< return a ==> f a where _ = noQuickCheck
-- warn "Monad law, right identity" = m >>= return ==> m where _ = noQuickCheck
-- warn "Monad law, right identity" = return =<< m ==> m where _ = noQuickCheck
-- warn = liftM ==> fmap
-- warn = liftA ==> fmap
-- hint = m >>= return . f ==> fmap f m where _ = noQuickCheck -- cannot be fmap, because is in Functor not Monad
-- hint = return . f =<< m ==> fmap f m where _ = noQuickCheck
-- warn = (if x then y else return ()) ==> Control.Monad.when x $ _noParen_ y where _ = not (isAtom y) && noQuickCheck
-- warn = (if x then y else return ()) ==> Control.Monad.when x y where _ = isAtom y && noQuickCheck
-- warn = (if x then return () else y) ==> Control.Monad.unless x $ _noParen_ y where _ = not (isAtom y) && noQuickCheck
-- warn = (if x then return () else y) ==> Control.Monad.unless x y where _ = isAtom y && noQuickCheck
-- warn = sequence (map f x) ==> mapM f x where _ = noQuickCheck
-- warn = sequence_ (map f x) ==> mapM_ f x where _ = noQuickCheck
-- hint = flip mapM ==> Control.Monad.forM where _ = noQuickCheck
-- hint = flip mapM_ ==> Control.Monad.forM_ where _ = noQuickCheck
-- hint = flip forM ==> mapM where _ = noQuickCheck
-- hint = flip forM_ ==> mapM_ where _ = noQuickCheck
-- warn = when (not x) ==> unless x where _ = noQuickCheck
-- warn = x >>= id ==> Control.Monad.join x where _ = noQuickCheck
-- warn = id =<< x ==> Control.Monad.join x where _ = noQuickCheck
-- hint = a >> return () ==> Control.Monad.void a
--     where _ = (isAtom a || isApp a) && noQuickCheck
-- warn = fmap (const ()) ==> Control.Monad.void where _ = noQuickCheck
-- warn = const () <$> x ==> Control.Monad.void x where _ = noQuickCheck
-- warn = flip (>=>) ==> (<=<) where _ = noQuickCheck
-- warn = flip (<=<) ==> (>=>) where _ = noQuickCheck
-- warn = flip (>>=) ==> (=<<) where _ = noQuickCheck
-- warn = flip (=<<) ==> (>>=) where _ = noQuickCheck
-- hint = (\x -> f x >>= g) ==> f Control.Monad.>=> g where _ = noQuickCheck
-- hint = (\x -> f =<< g x) ==> f Control.Monad.<=< g where _ = noQuickCheck
-- warn = a >> forever a ==> forever a where _ = noQuickCheck
-- hint = liftM2 id ==> ap where _ = noQuickCheck
-- warn = mapM (uncurry f) (zip l m) ==> zipWithM f l m where _ = noQuickCheck
-- warn = mapM_ (void . f) ==> mapM_ f
-- warn = mapM_ (void f) ==> mapM_ f
-- warn = forM_ x (void . f) ==> forM_ x f
-- warn = forM_ x (void f) ==> forM_ x f
-- warn = void (mapM f x) ==> mapM_ f x
-- warn = void (forM x f) ==> forM_ x f

-- -- STATE MONAD

-- warn = fst (runState x y) ==> evalState x y where _ = noQuickCheck
-- warn = snd (runState x y) ==> execState x y where _ = noQuickCheck

-- -- MONAD LIST

-- warn = fmap unzip (mapM f x) ==> Control.Monad.mapAndUnzipM f x where _ = noQuickCheck
-- warn = sequence (zipWith f x y) ==> Control.Monad.zipWithM f x y where _ = noQuickCheck
-- warn = sequence_ (zipWith f x y) ==> Control.Monad.zipWithM_ f x y where _ = noQuickCheck
-- warn = sequence (replicate n x) ==> Control.Monad.replicateM n x where _ = noQuickCheck
-- warn = sequence_ (replicate n x) ==> Control.Monad.replicateM_ n x where _ = noQuickCheck
-- warn = mapM f (replicate n x) ==> Control.Monad.replicateM n (f x) where _ = noQuickCheck
-- warn = mapM_ f (replicate n x) ==> Control.Monad.replicateM_ n (f x) where _ = noQuickCheck
-- warn = mapM f (map g x) ==> mapM (f . g) x where _ = noQuickCheck
-- warn = mapM_ f (map g x) ==> mapM_ (f . g) x where _ = noQuickCheck
-- warn = mapM id ==> sequence where _ = noQuickCheck
-- warn = mapM_ id ==> sequence_ where _ = noQuickCheck

-- -- APPLICATIVE / TRAVERSABLE

-- warn = flip traverse ==> for where _ = noQuickCheck
-- warn = flip for ==> traverse where _ = noQuickCheck
-- warn = flip traverse_ ==> for_ where _ = noQuickCheck
-- warn = flip for_ ==> traverse_ where _ = noQuickCheck
-- warn = foldr (*>) (pure ()) ==> sequenceA_ where _ = noQuickCheck
-- warn = foldr (<|>) empty ==> asum where _ = noQuickCheck
-- warn = liftA2 (flip ($)) ==> (<**>) where _ = noQuickCheck
-- warn = Just <$> a <|> pure Nothing ==> optional a where _ = noQuickCheck


-- -- LIST COMP

-- hint "Use list comprehension" = (if b then [x] else []) ==> [x | b]
-- hint "Redundant list comprehension" = [x | x <- y] ==> y where _ = isVar x

-- -- SEQ

-- warn "Redundant seq" = x `seq` x ==> x
-- warn "Redundant seq" = join seq ==> id
-- warn "Redundant $!" = id $! x ==> x
-- warn "Redundant seq" = x `seq` y ==> y where _ = isWHNF x
-- warn "Redundant $!" = f $! x ==> f x where _ = isWHNF x
-- warn "Redundant evaluate" = evaluate x ==> return x where _ = isWHNF x

-- -- TUPLE

-- warn = fst (unzip x) ==> map fst x
-- warn = snd (unzip x) ==> map snd x

-- -- MAYBE

-- warn = maybe x id ==> Data.Maybe.fromMaybe x
-- warn = maybe False (const True) ==> Data.Maybe.isJust
-- warn = maybe True (const False) ==> Data.Maybe.isNothing
-- warn = not (isNothing x) ==> isJust x
-- warn = not (isJust x) ==> isNothing x
-- warn = maybe [] (:[]) ==> maybeToList
-- warn = catMaybes (map f x) ==> mapMaybe f x
-- hint = (case x of Nothing -> y; Just a -> a)  ==> fromMaybe y x
-- warn = (if isNothing x then y else f (fromJust x)) ==> maybe y f x
-- warn = (if isJust x then f (fromJust x) else y) ==> maybe y f x
-- warn = maybe Nothing (Just . f) ==> fmap f
-- hint = map fromJust . filter isJust  ==>  Data.Maybe.catMaybes
-- warn = x == Nothing  ==>  isNothing x
-- warn = Nothing == x  ==>  isNothing x
-- warn = x /= Nothing  ==>  Data.Maybe.isJust x
-- warn = Nothing /= x  ==>  Data.Maybe.isJust x
-- warn = concatMap (maybeToList . f) ==> Data.Maybe.mapMaybe f
-- warn = concatMap maybeToList ==> catMaybes
-- warn = maybe n Just x ==> x Control.Applicative.<|> n
-- hint = (case x of Just a -> a; Nothing -> y)  ==> fromMaybe y x
-- warn = (if isNothing x then y else fromJust x) ==> fromMaybe y x
-- warn = (if isJust x then fromJust x else y) ==> fromMaybe y x
-- warn = isJust x && (fromJust x == y) ==> x == Just y
-- warn = mapMaybe f (map g x) ==> mapMaybe (f . g) x
-- warn = fromMaybe a (fmap f x) ==> maybe a f x
-- warn = mapMaybe id ==> catMaybes
-- hint = [x | Just x <- a] ==> Data.Maybe.catMaybes a
-- hint = (case m of Nothing -> Nothing; Just x -> x) ==> Control.Monad.join m
-- hint = maybe Nothing id ==> join
-- hint "Too strict maybe" = maybe (f x) (f . g) ==> f . maybe x g where note = IncreasesLaziness

-- -- EITHER

-- warn = [a | Left a <- a] ==> lefts a
-- warn = [a | Right a <- a] ==> rights a
-- warn = either Left (Right . f) ==> fmap f

-- -- INFIX

-- hint "Use infix" = elem x y ==> x `elem` y where _ = not (isInfixApp original) && not (isParen result)
-- hint "Use infix" = notElem x y ==> x `notElem` y where _ = not (isInfixApp original) && not (isParen result)
-- hint "Use infix" = isInfixOf x y ==> x `isInfixOf` y where _ = not (isInfixApp original) && not (isParen result)
-- hint "Use infix" = isSuffixOf x y ==> x `isSuffixOf` y where _ = not (isInfixApp original) && not (isParen result)
-- hint "Use infix" = isPrefixOf x y ==> x `isPrefixOf` y where _ = not (isInfixApp original) && not (isParen result)
-- hint "Use infix" = union x y ==> x `union` y where _ = not (isInfixApp original) && not (isParen result)
-- hint "Use infix" = intersect x y ==> x `intersect` y where _ = not (isInfixApp original) && not (isParen result)

-- -- MATHS

-- warn "Redundant fromIntegral" = fromIntegral x ==> x where _ = isLitInt x
-- warn "Redundant fromInteger" = fromInteger x ==> x where _ = isLitInt x
-- hint = x + negate y ==> x - y
-- hint = 0 - x ==> negate x
-- warn "Redundant negate" = negate (negate x) ==> x
-- hint = log y / log x ==> logBase x y
-- hint = sin x / cos x ==> tan x
-- hint = n `rem` 2 == 0 ==> even n
-- hint = n `rem` 2 /= 0 ==> odd n
-- hint = not (even x) ==> odd x
-- hint = not (odd x) ==> even x
-- hint = x ** 0.5 ==> sqrt x
-- hint "Use 1" = x ^ 0 ==> 1
-- hint = round (x - 0.5) ==> floor x

-- -- CONCURRENT

-- hint = mapM_ (writeChan a) ==> writeList2Chan a

-- -- EXCEPTION

-- hint = flip Control.Exception.catch ==> handle
-- hint = flip handle ==> Control.Exception.catch
-- hint = flip (catchJust p) ==> handleJust p
-- hint = flip (handleJust p) ==> catchJust p
-- hint = Control.Exception.bracket b (const a) (const t) ==> Control.Exception.bracket_ b a t
-- hint = Control.Exception.bracket (openFile x y) hClose ==> withFile x y
-- hint = Control.Exception.bracket (openBinaryFile x y) hClose ==> withBinaryFile x y
-- hint = throw (ErrorCall a) ==> error a
-- warn = toException NonTermination ==> nonTermination
-- warn = toException NestedAtomically ==> nestedAtomically

-- -- STOREABLE/PTR

-- hint = castPtr nullPtr ==> nullPtr
-- hint = castPtr (castPtr x) ==> castPtr x
-- hint = plusPtr (castPtr x) ==> plusPtr x
-- hint = minusPtr (castPtr x) ==> minusPtr x
-- hint = minusPtr x (castPtr y) ==> minusPtr x y
-- hint = peekByteOff (castPtr x) ==> peekByteOff x
-- hint = pokeByteOff (castPtr x) ==> pokeByteOff x

-- -- WEAK POINTERS

-- warn = mkWeak a a b ==> mkWeakPtr a b
-- warn = mkWeak a (a, b) c ==> mkWeakPair a b c

-- -- FOLDABLE

-- warn "Use Foldable.forM_" = (case m of Nothing -> return (); Just x -> f x) ==> Data.Foldable.forM_ m f
--     where _ = noQuickCheck
-- warn "Use Foldable.forM_" = when (isJust m) (f (fromJust m)) ==> Data.Foldable.forM_ m f
--     where _ = noQuickCheck

-- -- EVALUATE

-- -- TODO: These should be moved in to HSE\Evaluate.hs and applied
-- --       through a special evaluate hint mechanism
-- warn "Evaluate" = True && x ==> x
-- warn "Evaluate" = False && x ==> False
-- warn "Evaluate" = True || x ==> True
-- warn "Evaluate" = False || x ==> x
-- warn "Evaluate" = not True ==> False
-- warn "Evaluate" = not False ==> True
-- warn "Evaluate" = Nothing >>= k ==> Nothing
-- warn "Evaluate" = k =<< Nothing ==> Nothing
-- warn "Evaluate" = either f g (Left x) ==> f x
-- warn "Evaluate" = either f g (Right y) ==> g y
-- warn "Evaluate" = fst (x,y) ==> x
-- warn "Evaluate" = snd (x,y) ==> y
-- warn "Evaluate" = f (fst p) (snd p) ==> uncurry f p
-- warn "Evaluate" = init [x] ==> []
-- warn "Evaluate" = null [] ==> True
-- warn "Evaluate" = length [] ==> 0
-- warn "Evaluate" = foldl f z [] ==> z
-- warn "Evaluate" = foldr f z [] ==> z
-- warn "Evaluate" = foldr1 f [x] ==> x
-- warn "Evaluate" = scanr f z [] ==> [z]
-- warn "Evaluate" = scanr1 f [] ==> []
-- warn "Evaluate" = scanr1 f [x] ==> [x]
-- warn "Evaluate" = take n [] ==> [] where note = IncreasesLaziness
-- warn "Evaluate" = drop n [] ==> [] where note = IncreasesLaziness
-- warn "Evaluate" = takeWhile p [] ==> []
-- warn "Evaluate" = dropWhile p [] ==> []
-- warn "Evaluate" = span p [] ==> ([],[])
-- warn "Evaluate" = lines "" ==> []
-- warn "Evaluate" = unwords [] ==> ""
-- warn "Evaluate" = x - 0 ==> x
-- warn "Evaluate" = x * 1 ==> x
-- warn "Evaluate" = x / 1 ==> x
-- warn "Evaluate" = concat [a] ==> a
-- warn "Evaluate" = concat [] ==> []
-- warn "Evaluate" = zip [] [] ==> []
-- warn "Evaluate" = const x y ==> x

-- -- FOLDABLE + TUPLES

-- warn "Using foldr on tuple"   = foldr   f z (x,b) ==> f b z
-- warn "Using foldr' on tuple"  = foldr'  f z (x,b) ==> f b z
-- warn "Using foldl on tuple"   = foldl   f z (x,b) ==> f z b
-- warn "Using foldl' on tuple"  = foldl'  f z (x,b) ==> f z b
-- warn "Using foldMap on tuple" = foldMap f   (x,b) ==> f b
-- warn "Using foldr1 on tuple"  = foldr1  f   (x,b) ==> b
-- warn "Using foldl1 on tuple"  = foldl1  f   (x,b) ==> b
-- warn "Using elem on tuple"    = elem    e   (x,b) ==> e == b
-- warn "Using fold on tuple"    = fold        (x,b) ==> b
-- warn "Using toList on tuple"  = toList      (x,b) ==> b
-- warn "Using maximum on tuple" = maximum     (x,b) ==> b
-- warn "Using minimum on tuple" = minimum     (x,b) ==> b
-- warn "Using sum on tuple"     = sum         (x,b) ==> b
-- warn "Using product on tuple" = product     (x,b) ==> b
-- warn "Using concat on tuple"  = concat      (x,b) ==> b
-- warn "Using and on tuple"     = and         (x,b) ==> b
-- warn "Using or on tuple"      = or          (x,b) ==> b
-- warn "Using any on tuple"     = any     f   (x,b) ==> f b
-- warn "Using all on tuple"     = all     f   (x,b) ==> f b

-- warn "Using foldr on tuple"   = foldr   f z (x,y,b) ==> f b z
-- warn "Using foldr' on tuple"  = foldr'  f z (x,y,b) ==> f b z
-- warn "Using foldl on tuple"   = foldl   f z (x,y,b) ==> f z b
-- warn "Using foldl' on tuple"  = foldl'  f z (x,y,b) ==> f z b
-- warn "Using foldMap on tuple" = foldMap f   (x,y,b)   ==> f b
-- warn "Using foldr1 on tuple"  = foldr1  f   (x,y,b)   ==> b
-- warn "Using foldl1 on tuple"  = foldl1  f   (x,y,b)   ==> b
-- warn "Using elem on tuple"    = elem    e   (x,y,b)   ==> e == b
-- warn "Using fold on tuple"    = fold        (x,y,b)   ==> b
-- warn "Using toList on tuple"  = toList      (x,y,b)   ==> b
-- warn "Using maximum on tuple" = maximum     (x,y,b)   ==> b
-- warn "Using minimum on tuple" = minimum     (x,y,b)   ==> b
-- warn "Using sum on tuple"     = sum         (x,y,b)   ==> b
-- warn "Using product on tuple" = product     (x,y,b)   ==> b
-- warn "Using concat on tuple"  = concat      (x,y,b)   ==> b
-- warn "Using and on tuple"     = and         (x,y,b)   ==> b
-- warn "Using or on tuple"      = or          (x,y,b)   ==> b
-- warn "Using any on tuple"     = any     f   (x,y,b)   ==> f b
-- warn "Using all on tuple"     = all     f   (x,y,b)   ==> f b

-- warn "Using null on tuple"   = null x   ==> False where _ = isTuple x
-- warn "Using length on tuple" = length x ==> 1     where _ = isTuple x

-- To be able to use this as a test suite
main = putStrLn "I ran, ergo I compiled."
