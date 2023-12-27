{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module OpenProducts where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Fcf

data Any (f :: k -> Type) where
    Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where 
    OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

type UniqueKey (key :: k) (ts :: [(k, t)]) = Null =<< Filter (TyEq key <=< Fst) ts

insert :: Eval (UniqueKey key ts) ~ 'True => Key key -> f t -> OpenProduct f ts -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type FindElem (key :: Symbol) (ts :: [(Symbol, k)]) = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem :: forall key ts. KnownNat (FindElem key ts) => Int
findElem = fromIntegral . natVal $ Proxy @(FindElem key ts)

type LookupType (key :: k) (ts :: [(k, t)]) = FromMaybe Stuck =<< Lookup key ts

get :: forall key ts f. KnownNat (FindElem key ts) => Key key -> OpenProduct f ts -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem @key @ts
    where unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) = SetIndex (FindElem key ts) '(key, t) ts

update :: forall key ts t f. KnownNat (FindElem key ts) => Key key -> f t -> OpenProduct f ts -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) = OpenProduct $ v V.// [(findElem @key @ts, Any ft)]

delete :: forall key ts t f. KnownNat (FindElem key ts) => Key key -> OpenProduct f ts -> OpenProduct f (Eval(Filter (Not <=< TyEq key <=< Fst) ts))
delete _ (OpenProduct v) = let drInd = findElem @key @ts
                               (x, xs) = V.splitAt drInd v 
                           in OpenProduct $ (V.unsafeInit x) V.++ xs

test1 = insert (Key @"key") (Just 1) nil
test2 = delete (Key @"key") test1
--test4 = get (Key @"key") test2

--type Exists (key :: k) (ts :: [(k, t)])
{-
data FromMaybe :: k -> Maybe k -> Exp k
type instance Eval (FromMaybe a 'Nothing)   = a
type instance Eval (FromMaybe _a ('Just b)) = b
-}

data Exists :: Nat -> (Exp Bool)

type instance Eval (Exists _) = 'True

--exists :: forall key ts t f. KnownNat (FindElem key ts) => Key key -> OpenProduct f ts -> Exp Bool
--exists k ts = Eval (Exists k ts)

--test5 = Eval(Exists (Key @"key") test1)


--type instance Eval (Exists (Key key) (OpenProduct f ts) ('True)) = Stuck--Eval(FindIndex (TyEq key <=< Fst) ts)

--test5 = Eval (Exists (Key @"key") test1)
--test6 = Eval ( FromMaybe "nothing" ( 'Just " just " ) )
