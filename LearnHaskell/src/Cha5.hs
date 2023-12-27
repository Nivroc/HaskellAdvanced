{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module Cha5 where

import Data.Kind (Constraint, Type)

data HList (ts :: [Type]) where
    HNil :: HList '[]
    (:#) :: t -> HList ts -> HList (t ': ts)

infixr 5 :#

instance Eq (HList '[]) where
    HNil == HNil = True

instance (Eq t, Eq (HList ts)) => Eq (HList (t ': ts)) where
    (a :# as) == (b :# bs) = a == b && as == bs

instance Ord (HList '[]) where
    HNil `compare` HNil = EQ

instance (Ord t, Ord (HList ts)) => Ord (HList(t ': ts)) where
    (a :# as) `compare` (b :# bs) = case (a `compare` b) of
        EQ -> as `compare` bs
        x -> x

instance Show (HList '[]) where
    show HNil = ""

instance (Show t, Show (HList ts)) => Show (HList (t ': ts)) where
    show (a :# as) = show a <> " " <> show as

test1 = True :# False :# "Panties" :# HNil
test3 = True :# True :# "Panties" :# HNil
test2 = test1 `compare` test3

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5