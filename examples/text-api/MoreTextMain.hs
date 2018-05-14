import Test.Inspection
import MoreText
import ShouldDoes
import Data.List
import Data.Foldable
import Control.Monad

printResult (func, ress, s, d) = do
    putStrLn $ icon1 ++ icon2 ++ icon3 ++ icon4 ++ " " ++ func
    when (not (all (==d) ds) && any (==d) ds) $ forM_ ress $ \(p,r) ->
        case r of Success _ -> putStrLn $ "  ✓ " ++ p
                  Failure _ -> putStrLn $ "  ✗ " ++ p
  where
    icon1 = case s  of Should      -> "✓"
                       ShouldNot   -> "✗"
    icon2 = doesIcon d
    icon3 = doesIcon $ mconcat ds
    icon4 | May <- d      = " "
          | all (== d) ds = " "
          | any (== d) ds = "?"
          | otherwise     = "!"
    ds = map (toDoes . snd) ress

    doesIcon Does    = "✓"
    doesIcon May     = "?"
    doesIcon DoesNot = "✗"

toDoes (Failure _) = DoesNot
toDoes (Success _) = Does

main :: IO ()
main = mapM_ printResult tests
