module Scheduler where
import Data.Functor

class Functor m => CoMonad m where
    extract :: m a -> a
    wrap :: m a -> (m a -> b) -> m b
    wrap ma mab = mab <$> duplicate ma
    duplicate :: m a -> m (m a)


data Tree a = Leaf a | Branch (Tree a) a (Tree a)

instance Show a => Show (Tree a) where
    show (Leaf a) = "{Leaf:" <> show a <> "}"
    show (Branch l a r) = "{Branch: left:" <> show l <> "," <> "elem:" <> show a <> "," <> "right:" <> show r <> "}"
{-
extract . duplicate      = id
fmap extract . duplicate = id
duplicate . duplicate    = fmap duplicate . duplicate
-}

rrr :: Tree Int -> Int
rrr l@(Leaf i) = i
rrr (Branch l a r) = rrr r

instance Functor Tree where
    fmap ab (Leaf a) = Leaf $ ab a
    fmap ab (Branch l a r) = Branch (ab <$> l) (ab a) (ab <$> r)

instance CoMonad Tree where
    extract (Leaf a) = a
    extract (Branch _ a _) = a
    duplicate t@(Leaf a) = Leaf (Leaf a)
    duplicate t@(Branch l a r) = Branch (duplicate l) t (duplicate r)

zzz = Branch (Branch (Leaf 1) 2 (Leaf 3) ) 4 (Branch (Leaf 5) 6 (Leaf 7) )  

zz1 = (extract . duplicate) zzz
zz2 = (fmap extract . duplicate) zzz
zz3 = (duplicate . duplicate) zzz
zz4 = (fmap duplicate . duplicate) zzz

aa = wrap zzz rrr