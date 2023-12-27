{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Exercises where

import Data.Kind (Constraint, Type)

-- | Before we get started, let's talk about the @TypeOperators@ extension. All
-- this does is allow us to write types whose names are operators, and write
-- regular names as infix names with the backticks, as we would at the value
-- level.





{- ONE -}

data Nat = Z | S Nat

-- | a. Use the @TypeOperators@ extension to rewrite the 'Add' family with the
-- name '+':
type family (+) (x :: Nat) (y :: Nat) :: Nat where
  'Z  +  y = y
  ('S x) + y = 'S (x + y)

-- | b. Write a type family '**' that multiplies two naturals using '(+)'. Which
-- extension are you being told to enable? Why?

type family (**) (x :: Nat) (y :: Nat) :: Nat where
  ('Z) ** y = y
  ('S x) ** y = x ** (y + y)

-- | c. Write a function to add two 'SNat' values.
data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

addSnats :: SNat x -> SNat y -> SNat (x + y)
addSnats  SZ    y =           y
addSnats (SS x) y = SS (addSnats x y)



{- TWO -}

data Vector (count :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | a. Write a function that appends two vectors together. What would the size
-- of the result be?

append :: Vector m a -> Vector n a -> Vector (m + n) a
append  VNil        ys =                    ys
append (VCons x xs) ys = VCons x (append xs ys)

-- | b. Write a 'flatMap' function that takes a @Vector n a@, and a function
-- @a -> Vector m b@, and produces a list that is the concatenation of these
-- results. This could end up being a deceptively big job.

--flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
--flatMap VNil foo = VNil
--flatMap (VCons x xs) foo = (foo x) append (flatMap xs foo)
--flatMap :: Vector n a -> (a -> Vector m b) -> Vector (n ** m) b
--flatMap  VNil        _ = VNil
--flatMap (VCons x xs) f = append (f x) (flatMap xs f)





{- THREE -}

-- | a. More boolean fun! Write the type-level @&&@ function for booleans.
type family (&&)(x:: Bool)(y :: Bool) :: Bool where
  'True && 'True = 'True
  'False && 'False = 'False
  'True && 'False = 'False
  'False && 'True = 'False

-- | b. Write the type-level @||@ function for booleans.
type family (||)(x:: Bool)(y :: Bool) :: Bool where
  'True || 'True = 'True
  'False || 'False = 'False
  'True || 'False = 'True
  'False || 'True = 'True

-- | c. Write an 'All' function that returns @'True@ if all the values in a
-- type-level list of boleans are @'True@.
type family All (xs :: [Bool]) :: Bool where
  All '[     ]  = 'True
  All (x ': xs) =  x && All xs




{- FOUR -}

-- | a. Nat fun! Write a type-level 'compare' function using the promoted
-- 'Ordering' type.

type family Compare(a::Nat)(b::Nat)::Ordering where
  Compare 'Z 'Z = 'EQ
  Compare ('S a) 'Z = 'GT
  Compare 'Z ('S a) = 'LT
  Compare ('S a) ('S b) = Compare a b

-- | b. Write a 'Max' family to get the maximum of two natural numbers.
type family MaxHelper(a:: Nat)(b::Nat)(o:: Ordering):: Nat where
  MaxHelper x y 'EQ = x
  MaxHelper x y 'GT = x
  MaxHelper x y 'LT = y

type family Max(a::Nat)(b::Nat)::Nat where
  Max x y = MaxHelper x y (Compare x y)

-- | c. Write a family to get the maximum natural in a list.
type family MaxListHelper(acc :: Nat)(lst :: [Nat]):: Nat where
  MaxListHelper acc '[] = acc
  MaxListHelper acc (x ': xs) = MaxListHelper (Max acc x) xs
type family MaxList(lst::[Nat]):: Nat where
  MaxList a = MaxListHelper 'Z a


{- FIVE -}

data Tree = Empty | Node Tree Nat Tree

-- | Write a type family to insert a promoted 'Nat' into a promoted 'Tree'.
type family Promote (o:: Ordering)(a::Nat)(tr :: Tree):: Tree where
  Promote 'EQ a ('Node l e r) = 'Node l e r
  Promote 'GT a ('Node l e r) = Insert a r
  Promote 'LT a ('Node l e r) = Insert a l
type family Insert (a::Nat)(tr:: Tree):: Tree where
  Insert a 'Empty = 'Node 'Empty a 'Empty
  Insert a ('Node l e r) = Promote (Compare a e) a ('Node l e r) 




{- SIX -}

-- | Write a type family to /delete/ a promoted 'Nat' from a promoted 'Tree'.
type family Demote (o:: Ordering)(a::Nat)(tr :: Tree):: Tree where
  Demote 'EQ a ('Node l e r) = 'Node l e r
  Demote 'GT a ('Node l e r) = 'Node l e (Delete a r)
  Demote 'LT a ('Node l e r) = 'Node (Delete a l) e r
type family Recalc (l:: Tree)(r:: Tree):: Tree where
  Recalc 'Empty 'Empty = 'Empty
  Recalc 'Empty ('Node l a r) = 'Node l a r
  Recalc ('Node l a r) 'Empty  = 'Node l a r
  Recalc ('Node l1 a1 r1) ('Node l2 a2 r2)  = 'Node ('Node l1 a1 r1) a2 (Recalc l2 r2)
type family Delete (a::Nat)(tr:: Tree):: Tree where
  Delete a ('Node l a r) = Recalc l r
  Delete a ('Node l x r) = Demote (Compare a x) a ('Node l x r)
type family Leaf (a:: Nat):: Tree where
  Leaf a = 'Node 'Empty a 'Empty

type Zero = 'Z
type One = 'S 'Z
type Two = 'S ('S 'Z)
type OneTree = 'Node (Leaf Zero) One (Leaf Two)

data (x :: Tree) :~: (y :: Tree) where
  Refl :: x :~: x

deleteTestBase :: 'Empty :~: 'Empty
deleteTestBase = Refl

--deleteTestFail :: 'Empty :~: OneTree
--deleteTestFail = Refl


--deleteTest0 :: Delete Zero 'Empty :~: 'Empty
--deleteTest0 = Refl

deleteTest1 :: Delete Zero OneTree :~: 'Node 'Empty One (Leaf Two)
deleteTest1 = Refl

deleteTest2 :: Delete Two OneTree :~: 'Node (Leaf Zero) One 'Empty
deleteTest2 = Refl

deleteTest3 :: Delete One OneTree :~: 'Node (Leaf Zero) Two 'Empty
deleteTest3 = Refl

--deleteTest4:: Insert ('S 'Z) (Insert 'Z 'Empty) :~: 'Node 'Empty 'Z ('Node 'Empty ('S 'Z) 'Empty)
--deleteTest4 = Refl
{- SEVEN -}

-- | With @TypeOperators@, we can use regular Haskell list syntax on the
-- type-level, which I think is /much/ tidier than anything we could define.

data HList (xs :: [Type]) where
  HNil  :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- | Write a function that appends two 'HList's.
--appendX :: HList xs -> HList ys -> HList (xs ('++) ys)
type family (xs :: [Type]) ++ (ys :: [Type]) :: [Type] where
  '[     ]  ++ ys =             ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

appendH :: HList xs -> HList ys -> HList (xs ++ ys)
appendH  HNil        ys = ys
appendH (HCons x xs) ys = HCons x (appendH xs ys)



{- EIGHT -}

-- | Type families can also be used to build up constraints. There are, at this
-- point, a couple things that are worth mentioning about constraints:
--
-- - As we saw before, '()' is the empty constraint, which simply has "no
--   effect", and is trivially solved.
--
-- - Unlike tuples, constraints are "auto-flattened": ((a, b), (c, (d, ())) is
--   exactly equivalent to (a, b, c, d). Thanks to this property, we can build
--   up constraints using type families!

type family CAppend (x :: Constraint) (y :: Constraint) :: Constraint where
  CAppend x y = (x, y)

-- | a. Write a family that takes a constraint constructor, and a type-level
-- list of types, and builds a constraint on all the types.

type family Every (c :: Type -> Constraint) (x :: [Type]) :: Constraint where
  Every tc (x ': xs) = CAppend (tc x) (Every tc xs)

--data HList (xs :: [Type]) where
--  HNil  :: HList '[]
--  HCons :: x -> HList xs -> HList (x ': xs)
-- | b. Write a 'Show' instance for 'HList' that requires a 'Show' instance for
-- every type in the list.
instance (Every Show xx) => Show (HList xx) where
  show HNil = "[]"
  show (HCons x xs) = show x <> show xs
-- | c. Write an 'Eq' instance for 'HList'. Then, write an 'Ord' instance.
-- Was this expected behaviour? Why did we need the constraints?

instance (Every Eq xx) => Eq (HList xx) where
  HNil == HNil = True
  (HCons x xs) == (HCons y ys) = x == y && xs == ys

instance (Every Eq xs, Every Ord xs) => Ord (HList xs) where
  compare (HCons x xs) (HCons y ys) = compare x y <> compare xs ys
  compare  _            _           = EQ




{- NINE -}

-- | a. Write a type family to calculate all natural numbers up to a given
-- input natural.


-- | b. Write a type-level prime number sieve.

-- | c. Why is this such hard work?
