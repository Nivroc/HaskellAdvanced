{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cha1 where


-- ex1
f1 :: (b -> a) -> (c -> a) -> Either b c -> a
f1 ba ca ebc = either ba ca ebc

f2 :: (Either b c -> a) -> (b -> a, c -> a)
f2 ebca = (ebca . Left , ebca . Right )

-- ex2
ff1 :: (c -> (a, b)) -> (c -> a, c -> b)
ff1 cab = (fst . cab, snd . cab)


-- ex3
fff1 :: (c -> (b -> a)) -> ((b, c) -> a)
fff1 cba = \bc -> (cba $ snd bc) (fst bc) 

type family Or (x :: Bool) (y :: Bool) :: Bool where
    Or 'True y = 'True
    Or 'False y = y

type family Not (x :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True

newtype T1 a = T1 (Int -> a)
newtype T2 a = T2 (a -> Int)
newtype T3 a = T3 (a -> a)
newtype T4 a = T4 ((Int -> a) -> Int)
newtype T5 a = T5 ((a -> Int) -> Int)

instance Functor T1 where
    fmap ab (T1 ia) = T1 $ ab . ia

instance Functor T5 where
    fmap ab (T5 aii) = T5 $ \bi -> aii $ bi . ab