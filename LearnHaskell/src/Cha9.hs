{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cha9 where

import Data.Kind (Type)
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import GHC.TypeLits

data (a :: k1) :<< (b :: k2)
infixr 5 :<<

class HasPrintf a where
    type Printf a :: Type
    format :: String -> Proxy a -> Printf a

instance KnownSymbol text => HasPrintf (text :: Symbol) where
    type Printf text = String
    format str prox = str <> symbolVal (Proxy @text)
instance (KnownSymbol text, HasPrintf a) => HasPrintf ((text :: Symbol) :<< a) where
    type Printf (text :<< a) = Printf a
    format str prox = format (str <> symbolVal (Proxy @text)) (Proxy @a)
instance (HasPrintf a, Show param) => HasPrintf ((param :: Type) :<< a) where
    type Printf (param :<< a) = param -> Printf a
    format s _ param = format (s <> show param) (Proxy @a)

printf :: HasPrintf a => Proxy a -> Printf a
printf = format ""

tes1 = printf (Proxy @"hello")

tes2 = printf ( Proxy @( Int :<< "+" :<< Int :<< "=3") ) 1 2
