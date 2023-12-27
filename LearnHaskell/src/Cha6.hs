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

module Cha6 where

import Data.Functor

newtype Cont a = Cont { unCont :: forall r. (a -> r) -> r }

cont :: a -> (forall r. (a -> r) -> r)
cont a = \callback -> callback a

runCont :: (forall r. (a -> r) -> r) -> a
runCont f = let callback = id
            in f callback
{-
instance Functor Cont where
    fmap f fa = Cont $ cont $ f $ runCont $ unCont fa

instance Applicative Cont where
    pure a = Cont $ cont a
    fab <*> fa = Cont $ cont (f a)
        where f = runCont $ unCont fab
              a = runCont $ unCont fa

instance Monad Cont where
    ma >>= amb = amb a
        where a = runCont $ unCont ma
-}
instance Functor Cont where
    fmap f (Cont arr) = Cont $ \br -> arr (br . f)

instance Applicative Cont where
    pure a = Cont $ \ar -> ar a
    (Cont f) <*> (Cont a) = Cont $ \br -> br $ f a


test1 = pure 1 :: Cont Int
test2 = pure (\x -> x + 1) :: Cont (Int -> Int)

class Functor f => Ap f where
    pur :: f a
    litA2 :: (a -> b -> c) -> f a -> f b -> f c
    litA2 abc fa fb = ap (abc <$> fa) fb
    ap :: f (a -> b) -> f a -> f b 
    ap fab fa = litA2 (\f b -> f b) fab fa