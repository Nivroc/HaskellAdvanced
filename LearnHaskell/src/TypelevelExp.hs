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

module TypelevelExp where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import GHC.OverloadedLabels (IsLabel (..))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)
import Fcf
import Data.Typeable

type family BothEmpty (a :: [k]) (b :: [j]) :: Bool where
    BothEmpty '[] '[] = 'True
    BothEmpty _ _ = 'False

--foo :: forall a b i j. (BothEmpty a a ~ 'True) => a -> b -> Bool
--foo _ _ = True

type family Empty (a :: [k]) :: Bool where
    BothEmpty '[] = 'True
    BothEmpty _ = 'False

foo :: forall a. (Empty a ~ 'True) => Bool
foo = True

--t = foo @[1,2,3]

--foo2 :: forall a. (Empty ? ~ 'True) => [a] -> Bool
--foo2 = True
