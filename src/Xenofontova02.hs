{-# OPTIONS_GHC -Wall #-}
module Xenofontova02 where

-- Task 1
sumFl::[Integer] -> Integer
sumFl xs = foldl (+) 0 xs


-- Task 2
productFr::[Integer] -> Integer
productFr xs = if null xs then 0
               else foldr (*) 1 xs


-- Task 3
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs


-- Task 4
insertRec::[Int]->Int->[Int]->[Int]
insertRec xs1 v xs2
  |null xs1 = xs2++[v]
  |head xs1<v = insertRec (tail xs1) v (xs2++[head xs1])
  |otherwise = xs2++[v]++xs1

insert::[Int]->Int->[Int]
insert xs v = insertRec xs v []

sortInsert :: [Int] -> [Int]
sortInsert xs = foldl insert [] xs


-- Task 5
findIndices ::(Int -> Bool) -> [Int] -> [Int]
findIndices p xs = if null xs then []
                   else [ x | x<- [0,1..length xs-1],p (xs!!x)]


-- Task 6
reverseStrings ::String -> String
reverseStrings xs = if null xs then []
                    else reverseStrings (tail xs) ++ [head xs]

reverseListOfStrings::[String] -> [String]
reverseListOfStrings xs = if null xs then []
                          else reverseListOfStrings (tail xs) ++ [head xs]

allReverse ::[String] -> [String]
allReverse xs = if null xs then []
                else reverseListOfStrings [reverseStrings (xs !!x)| x<- [0,1..length xs-1]]


-- Task 7
isNotDigit::Char->Bool
isNotDigit x = not (x `elem` "0123456789")

noDigits :: String -> String
noDigits str = filter (isNotDigit) str


-- Task 8
cntGood :: [Int -> Bool] -> Int -> Int
cntGood xb x =  sum (map (\p -> if p x then 1 else 0) xb)


-- Task 9
next::[Integer]->[Integer]
next xs = zipWith (+) ([0] ++ xs) (xs ++ [0])

trianglePas :: [[Integer]]
trianglePas = iterate next [1]


-- Task 10
factorial::Integer -> Integer
factorial n = if n < 2 then 1 else n * factorial (n-1)

factorialsM :: [Integer]
--factorialsM =  zipWith (\x y -> factorial x + y) [1,2..] [0,0..] 
factorialsM = 1 : zipWith (*) factorialsM [2..]
              


