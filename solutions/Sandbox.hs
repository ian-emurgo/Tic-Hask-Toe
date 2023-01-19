{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

module Sandbox where

sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + sum' xs
 -- x0 + x1 + x2 + x3 ... + 0

product' :: [Int] -> Int
product' []     = 1
product' (x:xs) = x * product' xs

and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

or' :: [Bool] -> Bool
or' (x:xs) = x || or' xs
or' []     = False

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' q (x:xs) = x == q || elem' q xs

maximum', minimum' :: Ord a => [a] -> a
maximum' []     = error "empty list"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)
 -- max x y = if x > y then x else y

minimum' []     = error "empty list"
minimum' [x]    = x
minimum' (x:xs) = min x (minimum' xs)

reverse' = undefined


-- reverse "star"
--   reverse "tar" ++ "s"
--     (reverse "ar" ++ "t") ++ "s"
--      ((reverse "r" ++ "a") ++ "t") ++ "s"
--        (((reverse [] ++ "r") ++ "a") ++ "t") ++ "s"
--             [] ++ "r" ++ "a" ++ "t" ++ "s" ==> "rats"