{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module Exercises where


import Control.Arrow
import Control.Applicative


{- ONE -}

-- | Let's introduce a new class, 'Countable', and some instances to match.
class Countable a where count :: a -> Int
instance Countable Int  where count   = id
instance Countable [a]  where count   = length
instance Countable Bool where count x = if x then 1 else 0

-- | a. Build a GADT, 'CountableList', that can hold a list of 'Countable'
-- things.

data CountableList where
  CountNil :: CountableList
  CountCons :: Countable a => a -> CountableList -> CountableList


-- | b. Write a function that takes the sum of all members of a 'CountableList'
-- once they have been 'count'ed.

countList :: CountableList -> Int
countList = helper 0
  where helper acc = \case CountNil -> acc
                           CountCons a tail -> helper (acc + count a) tail


-- | c. Write a function that removes all elements whose count is 0.

dropZero :: CountableList -> CountableList
dropZero (CountCons a tail) = if count a == 0 then dropZero tail else CountCons a (dropZero tail)
dropZero x = x


-- | d. Can we write a function that removes all the things in the list of type
-- 'Int'? If not, why not?

filterInts :: CountableList -> CountableList
filterInts = error "Contemplate me!"





{- TWO -}

-- | a. Write a list that can take /any/ type, without any constraints.

data AnyList where
  AnyNil :: AnyList
  AnyCons :: a -> AnyList -> AnyList

-- | b. How many of the following functions can we implement for an 'AnyList'?

reverseAnyList :: AnyList -> AnyList
reverseAnyList AnyNil = AnyNil
reverseAnyList x = helper AnyNil x
  where helper lst = \case 
                        AnyNil -> lst
                        AnyCons a tail -> helper (AnyCons a lst) tail

--no
filterAnyList :: (a -> Bool) -> AnyList -> AnyList
filterAnyList = undefined

--yes
lengthAnyList :: AnyList -> Int
lengthAnyList = undefined

--no
foldAnyList :: Monoid m => AnyList -> m
foldAnyList = undefined

--yes
isEmptyAnyList :: AnyList -> Bool
isEmptyAnyList = undefined

--no
instance Show AnyList where
  show = error "What about me?"





{- THREE -}

-- | Consider the following GADT:

data TransformableTo output where
  TransformWith
    :: (input -> output)
    ->  input
    -> TransformableTo output

-- | ... and the following values of this GADT:

transformable1 :: TransformableTo String
transformable1 = TransformWith show 2.5

transformable2 :: TransformableTo String
transformable2 = TransformWith (uncurry (++)) ("Hello,", " world!")

-- | a. Which type variable is existential inside 'TransformableTo'? What is
-- the only thing we can do to it? input, we can only apply foo to it

-- | b. Could we write an 'Eq' instance for 'TransformableTo'? What would we be
-- able to check? Yes, only the outputs

-- | c. Could we write a 'Functor' instance for 'TransformableTo'? If so, write
-- it. If not, why not?
instance Functor TransformableTo where
  fmap f (TransformWith io i) = TransformWith (f . io) i





{- FOUR -}

-- | Here's another GADT:

data EqPair where
  EqPair :: Eq a => a -> a -> EqPair

-- | a. There's one (maybe two) useful function to write for 'EqPair'; what is
-- it? check if values are equal, map both values

-- | b. How could we change the type so that @a@ is not existential? (Don't
-- overthink it!)
data EqPair2 a where
  EqPair2 :: a -> a -> EqPair2 a

-- | c. If we made the change that was suggested in (b), would we still need a
-- GADT? Or could we now represent our type as an ADT?





{- FIVE -}

-- | Perhaps a slightly less intuitive feature of GADTs is that we can set our
-- type parameters (in this case @a@) to different types depending on the
-- constructor.

data MysteryBox a where
  EmptyBox  ::                                MysteryBox ()
  IntBox    :: Int    -> MysteryBox ()     -> MysteryBox Int
  StringBox :: String -> MysteryBox Int    -> MysteryBox String
  BoolBox   :: Bool   -> MysteryBox String -> MysteryBox Bool

-- | When we pattern-match, the type-checker is clever enough to
-- restrict the branches we have to check to the ones that could produce
-- something of the given type.

getInt :: MysteryBox Int -> Int
getInt (IntBox int _) = int

-- | a. Implement the following function by returning a value directly from a
-- pattern-match:

getInt' :: MysteryBox String -> Int
getInt' (StringBox str mi) = getInt mi

-- | b. Write the following function. Again, don't overthink it!

countLayers :: MysteryBox a -> Int
countLayers = helper 0
  where 
    helper :: Int -> MysteryBox a -> Int
    helper acc = \case
                        EmptyBox -> acc
                        IntBox _ b -> helper (acc + 1) b
                        StringBox _ b -> helper (acc + 1) b
                        BoolBox _ b -> helper (acc + 1) b

-- | c. Try to implement a function that removes one layer of "Box". For
-- example, this should turn a BoolBox into a StringBox, and so on. What gets
-- in our way? What would its type be?

data Layer a b where
  LayerUnit :: Layer Int ()
  LayerInt :: Layer Int String
  LayerString :: Layer String Bool

removeOne :: Layer a b -> MysteryBox a -> MysteryBox b
removeOne LayerUnit  (IntBox _ b) = b 
--removeOne (StringBox _ b) = b
--removeOne (BoolBox _ b) = b   



{- SIX -}

-- | We can even use our type parameters to keep track of the types inside an
-- 'HList'!  For example, this heterogeneous list contains no existentials:

data HList a where
  HNil  :: HList ()
  HCons :: head -> HList tail -> HList (head, tail)

exampleHList :: HList (String, (Int, (Bool, ())))
exampleHList = HCons "Tom" (HCons 25 (HCons True HNil))

-- | a. Write a 'head' function for this 'HList' type. This head function
-- should be /safe/: you can use the type signature to tell GHC that you won't
-- need to pattern-match on HNil, and therefore the return type shouldn't be
-- wrapped in a 'Maybe'!
head :: HList (a, b) -> a
head (HCons x _) = x

-- | b. Currently, the tuples are nested. Can you pattern-match on something of
-- type @HList (Int, String, Bool, ())@? Which constructor would work?

--patternMatchMe :: HList (Int, String, Bool, ()) -> Int
--patternMatchMe (HCons (x, y, z, i) _) = x

-- | c. Can you write a function that appends one 'HList' to the end of
-- another? What problems do you run into?





{- SEVEN -}

-- | Here are two data types that may help:

data Empty
data Branch left centre right

-- | a. Using these, and the outline for 'HList' above, build a heterogeneous
-- /tree/. None of the variables should be existential.

data HTree a where
  HEmpty :: HTree Empty
  HBranch :: HTree left -> centre -> HTree right -> HTree (Branch left centre right)

-- | b. Implement a function that deletes the left subtree. The type should be
-- strong enough that GHC will do most of the work for you. Once you have it,
-- try breaking the implementation - does it type-check? If not, why not?

deleteLeftSubtree :: HTree (Branch left centre right) -> HTree (Branch Empty centre right)
deleteLeftSubtree (HBranch _ c r) = HBranch HEmpty c r

-- | c. Implement 'Eq' for 'HTree's. Note that you might have to write more
-- than one to cover all possible HTrees. You might also need an extension or
-- two, so look out for something... flexible... in the error messages!
-- Recursion is your friend here - you shouldn't need to add a constraint to
-- the GADT!

instance Eq (HTree Empty) where
  (HEmpty) == (HEmpty) = True

instance (Eq (HTree l), Eq c, Eq (HTree r)) => Eq (HTree (Branch l c r)) where
  (HBranch l c r) == (HBranch l2 c2 r2) =  
    if c == c2 then False 
    else l == l2 && r == r2





{- EIGHT -}

-- | a. Implement the following GADT such that values of this type are lists of
-- values alternating between the two types. For example:
--
-- @
--   f :: AlternatingList Bool Int
--   f = ACons True (ACons 1 (ACons False (ACons 2 ANil)))
-- @

data AlternatingList a b where
  ANil :: AlternatingList a b
  ACons :: a -> AlternatingList b a -> AlternatingList a b

-- | b. Implement the following functions.

getFirsts :: AlternatingList a b -> [a]
getFirsts (ACons x (ACons _ tl)) = x : (getFirsts tl) 
getFirsts (ACons x ANil) = x : []
getFirsts ANil = []

getSeconds :: AlternatingList a b -> [b]
getSeconds (ACons x tl) = (getFirsts tl)
getSeconds (ANil) = []

-- | c. One more for luck: write this one using the above two functions, and
-- then write it such that it only does a single pass over the list

foldValues :: (Monoid a, Monoid b) => AlternatingList a b -> (a, b)
foldValues = (getFirsts &&& getSeconds) >>> (mconcat *** mconcat)

--test = (filter (>3) &&& filter (<3)) >>> (filter (>4) *** filter (<2))   $ [1,2,3,4,5]




{- NINE -}

-- | Here's the "classic" example of a GADT, in which we build a simple
-- expression language. Note that we use the type parameter to make sure that
-- our expression is well-formed.

data Expr a where
  Equals    :: Expr Int  -> Expr Int            -> Expr Bool
  Add       :: Expr Int  -> Expr Int            -> Expr Int
  If        :: Expr Bool -> Expr a   -> Expr a  -> Expr a
  IntValue  :: Int                              -> Expr Int
  BoolValue :: Bool                             -> Expr Bool

-- | a. Implement the following function and marvel at the typechecker:

eval :: Expr a -> a
eval (Equals x y) = eval x == eval y
eval (Add x y) = eval x + eval y
eval (If x y z) = if eval x then eval y else eval z
eval (IntValue x) = x
eval (BoolValue x) = x

-- | b. Here's an "untyped" expression language. Implement a parser from this
-- into our well-typed language. Note that (until we cover higher-rank
-- polymorphism) we have to fix the return type. Why do you think this is?

data DirtyExpr
  = DirtyEquals    DirtyExpr DirtyExpr
  | DirtyAdd       DirtyExpr DirtyExpr
  | DirtyIf        DirtyExpr DirtyExpr DirtyExpr
  | DirtyIntValue  Int
  | DirtyBoolValue Bool

parse :: DirtyExpr -> Maybe (Expr Int)
parse (DirtyAdd (DirtyIntValue x) (DirtyIntValue y)) = Just $ Add (IntValue x) (IntValue y)
parse (DirtyIf (DirtyBoolValue b) x y) = liftA2 (If (BoolValue b)) (parse x) (parse y)
parse (DirtyIntValue i) = Just $ IntValue i
parse _ = Nothing :: Maybe (Expr Int)

-- | c. Can we add functions to our 'Expr' language? If not, why not? What
-- other constructs would we need to add? Could we still avoid 'Maybe' in the
-- 'eval' function?





{- TEN -}

-- | Back in the glory days when I wrote JavaScript, I could make a composition
-- list like @pipe([f, g, h, i, j])@, and it would pass a value from the left
-- side of the list to the right. In Haskell, I can't do that, because the
-- functions all have to have the same type :(

-- | a. Fix that for me - write a list that allows me to hold any functions as
-- long as the input of one lines up with the output of the next.

data TypeAlignedList a b where
  TANil :: TypeAlignedList a a
  TACons :: (a -> b) -> TypeAlignedList b c -> TypeAlignedList a c

-- | b. Which types are existential?

test1 = TACons (<> "456") TANil
test2 = TACons (<> "456") test3
test3 = TACons (\x -> read x :: Integer) TANil

-- | c. Write a function to append type-aligned lists. This is almost certainly
-- not as difficult as you'd initially think.

composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
composeTALs t (TACons foo tail) = TACons foo (composeTALs t tail)
composeTALs t TANil = t

--composeTALs :: TypeAlignedList b c -> TypeAlignedList a b -> TypeAlignedList a c
--composeTALs xs  TANil        = xs
--composeTALs xs (TACons y ys) = TACons y (composeTALs xs ys)

