module Catalan where

import Data.List

data Br = Open | Close deriving (Show, Eq)

toCatalan :: Int -> Int
toCatalan x = length . (filter cond) . nub . permutations $ (replicate x Open) ++ (replicate x Close)
        where cond lst = (countCorrect 0 lst) && (length lst == 2 * x)

countCorrect :: Int -> [Br] -> Bool
countCorrect (-1) _ = False
countCorrect c (Open : xs) = countCorrect (c + 1) xs 
countCorrect c (Close : xs) = countCorrect (c - 1) xs 
countCorrect 0 [] = True
countCorrect _ [] = False

catalan x = take x (toCatalan <$> [0..])