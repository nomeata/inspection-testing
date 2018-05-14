import qualified TextFuns     as S
import qualified LazyTextFuns as L

import ShouldDoes
import Data.List
import Language.Haskell.TH (Name, nameBase)


stats untestable producers transformers consumers = unlines
    [ stat' "Total functions"             (untestable ++ allNames)
    , stat  "Untestable functions"        untestable
    , stat  "Fuses"                       fuses
    , stat  "Does not fuse (but should)"  negative
    , stat  "Fuses (but should not)"      positive
    , stat' "Does not fuse"               nofuse
    ]
  where
    allFuns = producers ++ transformers ++ consumers
    allNames = [ n | (n, _, _) <- allFuns ]
    fuses    = [ n | (n, Should, Does) <- allFuns ]
    negative = [ n | (n, Should, DoesNot) <- allFuns ]
    positive = [ n | (n, ShouldNot, Does) <- allFuns ]
    nofuse   = [ n | (n, ShouldNot, DoesNot) <- allFuns ]

stat, stat' :: String -> [Name] -> String
stat lbl xs = lbl ++ " (" ++ show (length xs) ++ "): " ++ intercalate ", " (map nameBase xs)
stat' lbl xs = lbl ++ " (" ++ show (length xs) ++ ")"


main = do
    putStrLn "Data.Text:"
    putStrLn $ stats S.untestable S.producers S.transformers S.consumers
    putStrLn "Data.Text.Lazy:"
    putStrLn $ stats L.untestable L.producers L.transformers L.consumers
