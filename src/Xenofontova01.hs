{-# OPTIONS_GHC -Wall #-}
module Xenofontova01 where

-- Task 1
power3::[Integer]
power3 = [x^3 | x<-[1,2 ..]]


-- Task 2
toPower3::[Integer]
toPower3 = [3^x | x<-[1,2 ..]]


-- Task 3
toPower3N::Integer->[Integer]
toPower3N n = [3^x | x<-[1,2 ..n]]

listToPower3::Integer -> [Integer]
listToPower3 x = if x<1 then error "x<1"
                 else toPower3N x

sumPower3::Integer -> Integer
sumPower3 n = sum (listToPower3 n)

-- Task 4
makePowerList::Integer->Integer->[Integer]
makePowerList x n = [x^y | y<-[1,2..n]]

listToPoweri::Integer->Integer->[Integer]
listToPoweri m n = if m<0 then error "m<0"
                 else makePowerList m n

sumPower::Integer->Integer->Integer
sumPower m n = if m<0 then 0
               else sum (listToPoweri m n)

-- Task 5

count::[Int]->Int->Int
count xs n
  | null xs = 0
  | head xs < n = 1 + count (tail xs) n
  | otherwise = count (tail xs) n

lessMe::[Int]->[Int]
lessMe xs = if null xs then error "list is empty"
            else [count xs (xs !!x)| x<- [0,1..length xs-1] ]



-- Task 6
similarityCount::[Int]->Int->Int
similarityCount xs n
  | null xs = 0
  | head xs == n = 1 + similarityCount (tail xs) n
  | otherwise = similarityCount (tail xs) n
   
  
allFrequency::[Int] -> [(Int,Int)]
allFrequency xs = if null xs then error "list is empty"
                  else  [(,)(xs !!x) (similarityCount xs (xs!!x)) | x<-[0,1..length xs-1]]


removeItem :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
removeItem _ []                 = []
removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys
   
   
removePair :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
removePair xs p = if null xs then error "list is empty"
                        else removeItem p xs

                    
resList :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
resList xs list = if null list then xs
                    else resList ((head list):xs) (removePair list (head list))


frequency::[Int] -> [(Int,Int)]
frequency xs = if null xs then error "list is empty"
                else reverse (resList [] (allFrequency xs))



-- Task 7
hailstone::Int->Int
hailstone n
  | n == 1 = 1
  | n <= 0 = error "invalid input number"
  | even n = div n 2
  | otherwise = n * 3 + 1


-- Task 8
reverseList :: [Int] -> [Int]
reverseList xs =
  if null xs
    then []
    else reverseList (tail xs) ++ [head xs]

hailList::[Int]->Int->[Int]
hailList xs n = if n==1 then 1:xs
                else hailList(n:xs)(hailstone n)


hailSeq::Int->[Int]
hailSeq n = if n<=0 then []
            else reverseList (hailList [n] (hailstone n))


-- Task 9
allHailSeq::[[Int]]
allHailSeq = [if x==1 then [1] else hailSeq x | x<-[1,2 ..]]


-- Task 10
takeListHailSeq::[[Int]] -> Int ->Int
takeListHailSeq xs l = if length (head xs) == l then head (head xs)
                           else takeListHailSeq (tail xs) l


firstHailSeq::Int -> Int
firstHailSeq l = takeListHailSeq allHailSeq l

  
  