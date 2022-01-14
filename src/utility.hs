module Utility where

customInit::[a]->[a]
customInit [] = []
customInit [x] = []
customInit (x:xs) = x:customInit xs 

countOccurences :: Eq a => a -> [a] -> Int
countOccurences elem l = foldr (\x -> if x == elem then (1+) else (0+)) 0 l

getElementIndex :: Eq a => a -> [a] -> Int -> Int
getElementIndex elem [] index = -1
getElementIndex elem (x:xs) index
        | x == elem      = index
        | otherwise      = getElementIndex elem xs (index + 1)

getElement :: Eq a => a -> [a] -> Int
getElement elem l = getElementIndex elem l 0