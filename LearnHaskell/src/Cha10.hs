{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Cha10 where
import Data.Kind (Constraint, Type)
{-

class Eval l t | l -> t where 
    eval :: l -> t

data LstToMby a = LstToMby [a]

listToMaybe :: [a] -> Maybe a
listToMaybe = undefined


testt = eval (LstToMby [])

instance Eval (LstToMby a) (Maybe a) where
    eval (LstToMby (x:xs)) = Just x
    eval (LstToMby []) = Nothing 
-}
type Exp a = a -> Type
type family Eval (e :: Exp a) :: a

data Ltm :: [a] -> Exp (Maybe a)
type instance Eval (Ltm '[]) = Nothing
type instance Eval (Ltm (x ': xs)) = Just x

--foldr :: (a -> b -> b) -> b -> [a] -> b

data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b

type instance Eval (FoldR _1 b '[]) = b
type instance Eval (FoldR f acc (x ': as)) = Eval (f x (Eval (FoldR f x as)))

data Trans1 :: a -> b -> Exp b
type instance Eval (Trans1 a b) = b

data Fst :: (a, b) -> Exp a
type instance Eval(Fst '(a, _)) = a
data Snd :: (a, b) -> Exp b
type instance Eval(Snd '(_, a)) = a

data Map :: (a -> Exp b) -> f a -> Exp (f b)

type instance Eval (Map f '[]) = '[]
type instance Eval (Map f (a ': as)) = Eval (f a) ': Eval (Map f as)

type instance Eval (Map f '(x, y)) = '(Eval (f x), Eval (f y))