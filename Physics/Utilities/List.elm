module Physics.Utilities.List where

import Physics.Utilities.Basics (..)

-- Turns an object into a list with one element
toList : a -> [a]
toList = (flip (::)) []

-- Function to take the nth element of a list
takeNth : Int -> [a] -> a
takeNth n =  last . take n

-- Function to remove the nth element from a list
removeNth : Int -> [a] -> [a]
removeNth n list = (take (n-1) list) ++ (drop n list)

{- Function to apply a binary function to an element
   of a list and all other elements of that list
   except itself -}
applyNth : Int -> (a -> a -> b) -> [a] -> [b]
applyNth n f list = map ((f . takeNth n) list) (removeNth n list)

{- Function to apply a binary function to an element
   of a list and all other elements of that list 
   except itself for all elements of the list -}
applyAll : (a -> a -> b) -> [a] -> [[b]]
applyAll f list = map ((toFront_3 applyNth) f list) [1..(length list)]