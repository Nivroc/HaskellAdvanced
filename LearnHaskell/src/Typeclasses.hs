{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Typeclasses where


data Foo = Bar | Baz

class Clz a where
    prnt :: Int

instance Clz Foo where
    prnt = 1

ttee :: (Clz a) => a -> Int
ttee a = prnt @ (a)