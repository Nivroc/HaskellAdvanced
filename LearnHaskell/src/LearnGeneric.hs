{-# LANGUAGE DeriveGeneric, FlexibleContexts, DerivingVia #-}

module LearnGeneric where
import GHC.Generics
{-
data B = T | F deriving (Generic)

class GEq a where
    geq :: a x -> a x -> Bool

class GEq a => GOrd a where
    gord :: a x -> a x -> Ordering

instance GEq U1 where
    geq U1 U1 = True

instance GOrd U1 where
    gord U1 U1 = EQ

instance GEq V1 where
    geq _ _ = True

instance GOrd V1 where
    gord _ _ = EQ

instance Eq a => GEq (K1 _1 a) where
    geq (K1 a) (K1 b) = a == b

instance Ord a => GOrd (K1 _1 a) where
    gord (K1 a) (K1 b) = a `compare` b

instance (GEq a, GEq b) => GEq (a :+: b) where
    geq (L1 a1) (L1 a2) = geq a1 a2
    geq (R1 b1) (R1 b2) = geq b1 b2
    geq _ _ = False

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
    gord (L1 a1) (L1 a2) = gord a1 a2
    gord (R1 b1) (R1 b2) = gord b1 b2
    gord _ (R1 _) = LT
    gord _ _ = GT

instance (GEq a, GEq b) => GEq (a :*: b) where
    geq (a1 :*: b1) (a2 :*: b2) = geq a1 a2 && geq b1 b2


instance (GOrd a, GOrd b) => GOrd (a :*: b) where
    gord (a1 :*: b1) (a2 :*: b2) = if geq a1 a2 == True
                                   then gord b1 b2
                                   else gord a1 a2                 

instance GEq a => GEq (M1 _x _y a) where
    geq (M1 a1) (M1 a2) = geq a1 a2

instance GOrd a => GOrd (M1 _x _y a) where
    gord (M1 a1) (M1 a2) = gord a1 a2


genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericOrd a b = gord (from a) (from b)


exNihilo :: Maybe a
exNihilo (Just)

-}
