{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Exercises where

import Data.Kind (Type)
import Data.Function ((&))
import Data.Kind




{- ONE -}

-- | One of the restrictions around classes that we occasionally hit is that we
-- can only have one instance for a type. There are, for example, two good
-- candidates for a monoid instance when we think about 'Integer':

data IntegerMonoid = Sum | Product

-- | a. Write a newtype around 'Integer' that lets us choose which instance we
-- want.
newtype SumMon (b :: IntegerMonoid) = Mon {a :: Int}
-- | b. Write the two monoid instances for 'Integer'.
instance Semigroup (SumMon 'Sum) where
  (<>) (Mon a) (Mon b) = Mon(a + b)
instance Semigroup (SumMon 'Product) where
  (<>) (Mon a) (Mon b) = Mon(a * b)

instance Show (SumMon a) where
  show (Mon a) = (show a) :: String

test = (Mon 1 <> Mon 2) :: (SumMon 'Product)
--instance Monoid (SumMon 'Sum) where
-- | c. Why do we need @FlexibleInstances@ to do this?





{- TWO -}

-- | We can write a type that /is/ of kind 'Type', but has no value-level
-- members. We usually call this type 'Void':

data Void -- No constructors!

-- | a. If we promote this with DataKinds, can we produce any /types/ of kind
-- 'Void'?

-- | b. What are the possible type-level values of kind 'Maybe Void'?

-- | c. Considering 'Maybe Void', and similar examples of kinds such as
-- 'Either Void Bool', why do you think 'Void' might be a useful kind?





{- THREE -}

-- | a. Write a GADT that holds strings or integers, and keeps track of how
-- many strings are present. Note that you might need more than 'Nil' and
-- 'Cons' this time...

data Nat = Z | S Nat

data StringAndIntList (stringCount :: Nat) where
  SaiNil :: StringAndIntList 'Z
  SaiString :: String -> StringAndIntList a -> StringAndIntList ('S a)
  SaiInt :: Int -> StringAndIntList a -> StringAndIntList a

-- | b. Update it to keep track of the count of strings /and/ integers.
data UStringAndIntList (stringCount :: Nat)(intCount :: Nat) where
  USaiNil :: UStringAndIntList 'Z 'Z
  USaiString :: String -> UStringAndIntList a b -> UStringAndIntList ('S a) b
  USaiInt :: Int -> UStringAndIntList a b -> UStringAndIntList a ('S b)

-- | c. What would be the type of the 'head' function?

uhead :: UStringAndIntList ('S a) ('S b) -> (Int, String)
uhead _ = undefined




{- FOUR -}

-- | When we talked about GADTs, we discussed existentials, and how we could
-- only know something about our value if the context told us:

data Showable where
  Showable :: Show a => a -> Showable

-- | a. Write a GADT that holds something that may or may not be showable, and
-- stores this fact in the type-level.

data MaybeShowable (isShowable :: Bool) where
  IsShowable :: (Show a) => a -> MaybeShowable 'True
  NotShowable :: a -> MaybeShowable 'False

-- | b. Write a 'Show' instance for 'MaybeShowable'. Your instance should not
-- work unless the type is actually 'show'able.
instance Show (MaybeShowable 'True) where
  show (IsShowable a) = show a

-- | c. What if we wanted to generalise this to @Constrainable@, such that it
-- would work for any user-supplied constraint of kind 'Constraint'? How would
-- the type change? What would the constructor look like? Try to build this
-- type - GHC should tell you exactly which extension you're missing.
data Constrainable (c :: Type -> Constraint) where
  Constrained :: c x => x -> Constrainable c




{- FIVE -}

-- | Recall our list type:

data List a = Nil | Cons a (List a)

-- | a. Use this to write a better 'HList' type than we had in the @GADTs@
-- exercise. Bear in mind that, at the type-level, 'Nil' and 'Cons' should be
-- "ticked". Remember also that, at the type-level, there's nothing weird about
-- having a list of types!

data HList (length :: Nat) (types :: List Type) where
  HNil  :: HList 'Z 'Nil
  HCons :: a -> HList l prev -> HList ('S l) ('Cons a prev)  

-- | b. Write a well-typed, 'Maybe'-less implementation for the 'tail' function
-- on 'HList'.

--htail :: HList ('Cons a tl) -> HList tl  
--htail (HCons _ tl) = tl

-- | c. Could we write the 'take' function? What would its type be? What would
-- get in our way?
--htake :: Nat n => n -> HList (length :: Nat) (types :: List Type)
--htake n lst = 




{- SIX -}

-- | Here's a boring data type:

data BlogAction
  = AddBlog
  | DeleteBlog
  | AddComment
  | DeleteComment

data SecurityLevel = User | Admin
-- | a. Two of these actions, 'DeleteBlog' and 'DeleteComment', should be
-- admin-only. Extend the 'BlogAction' type (perhaps with a GADT...) to
-- express, at the type-level, whether the value is an admin-only operation.
-- Remember that, by switching on @DataKinds@, we have access to a promoted
-- version of 'Bool'!
data SecureBlogAction (securityLevel :: SecurityLevel) where
  SAddBlog :: SecureBlogAction 'User
  SDeleteBlog :: SecureBlogAction 'Admin
  SAddComment :: SecureBlogAction 'User
  SDeleteComment :: SecureBlogAction 'Admin

-- | b. Write a 'BlogAction' list type that requires all its members to be
-- the same "access level": "admin" or "non-admin".

--newtype BlogActionList (isSafe :: Bool) = BlogActionList [BlogAction' isSafe]

-- | c. Let's imagine that our requirements change, and 'DeleteComment' is now
-- available to a third role: moderators. Could we use 'DataKinds' to introduce
-- the three roles at the type-level, and modify our type to keep track of
-- this?





{- SEVEN -}

-- | When we start thinking about type-level Haskell, we inevitably end up
-- thinking about /singletons/. Singleton types have a one-to-one value-type
-- correspondence - only one value for each type, only one type for each value.
-- A simple example is '()', whose only value is '()'. 'Bool' is /not/ a
-- singleton, because it has multiple values.

-- We can, however, /build/ a singleton type for 'Bool':
data SBool (value :: Bool) where
  SFalse :: SBool 'False
  STrue  :: SBool 'True

instance Show (SBool n)  where
  show SFalse = "False"
  show STrue  = "True"

-- | a. Write a singleton type for natural numbers:

data SNat (value :: Nat) where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

showSNat :: SNat n -> Int
showSNat (SS n) = 1 + showSNat n
showSNat SZ = 0


-- | b. Write a function that extracts a vector's length at the type level:

length2 :: Vector n a -> SNat n
length2  VNil        = SZ
length2 (VCons x xs) = SS (length2 xs)

-- | c. Is 'Proxy' a singleton type?

data Proxy a = Proxy





{- EIGHT -}

-- | Let's imagine we're writing some Industry Haskell™, and we need to read
-- and write to a file. To do this, we might write a data type to express our
-- intentions:

data Program                     result
  = OpenFile            (Program result)
  | WriteFile  String   (Program result)
  | ReadFile  (String -> Program result)
  | CloseFile (          Program result)
  | Exit                         result

-- | We could then write a program like this to use our language:

myApp :: Program Bool
myApp
  = OpenFile $ WriteFile "HEY" $ (ReadFile $ \contents ->
      if contents == "WHAT"
        then WriteFile "... bug?" $ Exit False
        else CloseFile            $ Exit True)

-- | ... but wait, there's a bug! If the contents of the file equal "WHAT", we
-- forget to close the file! Ideally, we would like the compiler to help us: we
-- could keep track of whether the file is open at the type level!
--
-- - We should /not/ be allowed to open a file if another file is currently
-- open.
--
-- - We should /not/ be allowed to close a file unless a file is open.
--
-- If we had this at the type level, the compiler should have been able to tell
-- us that the branches of the @if@ have different types, and this program
-- should never have made it into production. We should also have to say in the
-- type of 'myApp' that, once the program has completed, the file will be
-- closed.

-- | Improve the 'Program' type to keep track of whether a file is open.  Make
-- sure the constructors respect this flag: we shouldn't be able to read or
-- write to the file unless it's open. This exercise is a bit brain-bending;
-- why? How could we make it more intuitive to write?


data SProgram  (isClosed :: Bool) result where
  SOpenFile :: (SProgram 'True result) -> (SProgram 'False result)
  SWriteFile :: String  -> (SProgram 'True result) -> (SProgram 'True result)
  SReadFile :: (String -> SProgram 'True result) -> SProgram 'True result
  SCloseFile :: (SProgram 'False result)  -> (SProgram 'True result)
  SExit :: result -> (SProgram 'False result)

-- | EXTRA: write an interpreter for this program. Nothing to do with data
-- kinds, but a nice little problem.

interpret :: SProgram any a -> IO a

interpret (SOpenFile next)
  = putStrLn "Opened file..." >> interpret next

interpret (SWriteFile output next)
  = putStrLn ("Writing " <> output <> "...") >> interpret next

interpret (SReadFile k)
  = interpret (k "Some file contents")

interpret (SCloseFile next)
  = putStrLn "Closing file..." >> interpret next

interpret (SExit x)
  = putStrLn "Goodbye!" >> pure x
  

test2 = interpret $ SOpenFile (SCloseFile (SExit "123"))



{- NINE -}

-- | Recall our vector type:

data Vector (n :: Nat) (a :: Type) where
  VNil  :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

-- | Imagine we want to write the '(!!)' function for this vector. If we wanted
-- to make this type-safe, and avoid 'Maybe', we'd have to have a type that can
-- only hold numbers /smaller/ than some type-level value.

-- | a. Implement this type! This might seem scary at first, but break it down
-- into Z and S cases. That's all the hint you need :)

data SmallerThan (limit :: Nat) where
  STZ :: SmallerThan ('S n)
  STN :: SmallerThan n -> SmallerThan ('S n)

test3 = S Z

-- | b. Write the '(!!)' function:

blabla :: Vector n a -> SmallerThan n -> a
blabla (VCons x _) (STN STZ) = x
blabla (VCons _ tail) (STN n) = blabla tail n

testv = VCons 44 $ VCons 13 $ VCons 1 VNil
testSTZ = STN STZ
-- | c. Write a function that converts a @SmallerThan n@ into a 'Nat'.

convert :: SmallerThan n -> Nat
convert STZ = Z
convert (STN n) = S (convert n)

testf = convert testSTZ