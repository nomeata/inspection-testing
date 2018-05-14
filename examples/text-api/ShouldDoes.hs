{-# LANGUAGE DeriveLift #-}
module ShouldDoes where

import Language.Haskell.TH.Syntax

data Should = Should | ShouldNot   deriving (Show, Eq, Ord, Lift)
data Does   = Does | May | DoesNot deriving (Show, Eq, Ord, Lift)

instance Monoid Should where
    mempty = Should
instance Monoid Does where
    mempty = Does
instance Semigroup Should where
    (<>) = max
instance Semigroup Does where
    (<>) = max

s_d_ x = (x, Should,    Does)
snd_ x = (x, ShouldNot, Does)
s_dn x = (x, Should,    DoesNot)
sndn x = (x, ShouldNot, DoesNot)

