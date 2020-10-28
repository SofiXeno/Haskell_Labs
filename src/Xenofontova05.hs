{-# OPTIONS_GHC -Wall #-}

module Xenofontova05 where

import Data.Char(isUpper)
import Data.List

type Grammar    = [Production]         -- КВ-граматика
type Production = (Char,String)        -- Правило виводу
type Predict    = [(Char,String)]      -- Прогнозуюча таблиця
type Control    = [((Char,Char),Int)]  -- Управляюча таблиця

-- Задача 1 ------------------------------------
addOne :: String -> Char -> String
addOne st c =
  let n = findSymbol st c 0
   in if n == (-1)
        then st ++ [c]
        else st

findSymbol :: String -> Char -> Int -> Int
findSymbol [] _ _ = -1
findSymbol (x:st) c n
  | x == c = n
  | otherwise = findSymbol st c (n + 1)

addAll :: String -> String -> String
addAll st [] = st
addAll st wd =
  let line = removeDuplicates wd
   in if findSymbol st (head line) 0 == (-1)
        then addAll (st ++ [head line]) (drop 1 line)
        else addAll st (drop 1 line)

removeDuplicates :: String -> String
removeDuplicates [] = []
removeDuplicates (x:xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = x : removeDuplicates xs

addWithout :: String -> String -> String
addWithout st [] = st
addWithout st wd = addAll st (filter (/= '$') wd)

inter :: String -> String -> String
inter [] _ = []
inter (x:xs) l
  | x `elem` l = x : inter xs (delete x l)
  | otherwise = inter xs l

-- Задача 2 ------------------------------------
tkPredict :: Predict -> Char -> String
tkPredict [] _ = ""
tkPredict pt n =
  if fst (head pt) == n
    then snd (head pt)
    else tkPredict (drop 1 pt) n

upPredict :: Predict -> Char -> String -> Predict
upPredict pt n st =
  let index = findNoTerminalIndex pt n 0
   in if index == (-1)
        then insertionSort (pt ++ [(n, st)])
        else insertionSort (take index pt ++ [(n, st)] ++ drop (index + 1) pt)

findNoTerminalIndex :: Predict -> Char -> Int -> Int
findNoTerminalIndex [] _ _ = -1
findNoTerminalIndex pt n num =
  if fst (head pt) == n
    then num
    else findNoTerminalIndex (drop 1 pt) n (num + 1)

insertionSort :: (Ord a) => [a] -> [a]
insertionSort = foldr insert []

---- Задача 3 ------------------------------------
parse ::  Grammar -> Control -> String -> Maybe [Int]
parse = undefined

step :: Grammar -> Control ->
       (String, String, Maybe [Int]) -> (String, String, Maybe [Int])
step = undefined

getFirst :: (String, String, Maybe [Int]) -> String
getFirst (x, _, _) = x

getSecond :: (String, String, Maybe [Int]) -> String
getSecond (_, x, _) = x

getThird :: (String, String, Maybe [Int]) -> Maybe [Int]
getThird (_, _, x) = x

specifyJust :: Maybe a -> a
specifyJust (Just x) = x
specifyJust Nothing  = undefined

-- Задача 4 ------------------------------------
first :: Predict -> String -> String
first pFst st
  | null st = "$"
  | not (isUpper (head st)) = [head st]
  | length st == 1 = curPredict
  | '$' `notElem` curPredict = curPredict
  | otherwise = insertionSort (addWithout (first pFst (tail st)) curPredict)
  where
    curPredict = snd (head [n | n <- pFst, fst n == head st])

-- Задача 5 ------------------------------------
buildingControl :: Grammar -> Predict -> Predict -> Control
buildingControl gr pFst pNxt = sortBy (\(a, _) (b, _) -> compare a b) (buildTerm gr pFst pNxt 0)

buildTerm :: Grammar -> Predict -> Predict -> Int -> Control
buildTerm gr pFst pNxt n
  | null gr = []
  | chPFst /= "$" = built ++ buildCL1 (fst (head gr)) chPFst pFst n
  | chPFst == "$" = built ++ buildCL1 (fst (head gr)) (tkPredict pNxt (fst (head gr))) pNxt n
  | otherwise = built
  where
    chPFst = first pFst (snd (head gr))
    built = buildTerm (tail gr) pFst pNxt (n + 1)



buildCL1 :: Char -> String -> Predict -> Int -> [((Char, Char), Int)]
buildCL1 ch str pr n = if fst (head pr) == ch then [((ch, x), n) | x <- str]
                       else buildCL1 ch str (tail pr) n

-- Задача 6 ------------------------------------
testingLL1 :: Grammar -> Predict -> Predict -> Bool
testingLL1 = undefined

fromGrammar :: Grammar -> [(Char, [String])]
fromGrammar = undefined

testFst :: [String] -> Bool
testFst = undefined

testFollow :: String -> [String] -> Bool
testFollow = undefined

-- Задача 7 ------------------------------------
buildFst :: Grammar -> Predict
buildFst = undefined

evalFst :: Grammar -> Predict -> Predict
evalFst = undefined

extandFst :: Predict -> Production -> Predict
extandFst = undefined

-- Задача 8 ------------------------------------
buildNxt :: Grammar -> Predict -> Predict
buildNxt = undefined

nontermTails :: Grammar -> [(Char, String)]
nontermTails = undefined

evalNxt :: [(Char, String)] -> Predict -> Predict -> Predict
evalNxt = undefined

extandNxtOne :: Predict -> Char -> Predict -> String -> Predict
extandNxtOne = undefined

---------------------Тестові дані ---------------------------
gr0, gr1, gr2, gr3, gr4, gr5 :: Grammar
--  LL(1)-граматики
gr0 = [('S', "aAS"), ('S', "b"), ('A', "a"), ('A', "bSA")]

gr1 = [('S', "TV"), ('T', "d"), ('T', "(S)"), ('V', "+TV"), ('V', "-TV"), ('V', "")]

gr2 =
  [ ('E', "TU")
  , ('U', "")
  , ('U', "+TU")
  , ('U', "-TU")
  , ('T', "FV")
  , ('V', "")
  , ('V', "*FV")
  , ('V', "%FV")
  , ('V', "/FV")
  , ('F', "d")
  , ('F', "(E)")
  ]

-- не LL(1)-граматики
gr3 = [('S', "aAS"), ('S', "a"), ('A', "SbA"), ('A', "ba"), ('S', "")]

gr4 = [('E', "E+T"), ('E', "T"), ('T', "T*F"), ('T', "F"), ('F', "d"), ('F', "(E)")]

gr5 =
  [ ('E', "E+T")
  , ('E', "E-T")
  , ('E', "T")
  , ('T', "T*F")
  , ('T', "T%F")
  , ('T', "T/F")
  , ('T', "F")
  , ('F', "d")
  , ('F', "(E)")
  ]

-- прогнозуючі таблиці початкових терміналів Fst
pFst0, pFst1, pFst2, pFst3, pFst4, pFst5 :: Predict
pFst0 = [('A', "ab"), ('S', "ab")]

pFst1 = [('S', "(d"), ('T', "(d"), ('V', "$+-")]

pFst2 = [('E', "(d"), ('F', "(d"), ('T', "(d"), ('U', "$+-"), ('V', "$%*/")]

pFst3 = [('A', "ab"), ('S', "$a")]

pFst4 = [('E', "(d"), ('F', "(d"), ('T', "(d")]

pFst5 = [('E', "(d"), ('F', "(d"), ('T', "(d")]

-- прогнозуючі таблиці наступних терміналів Nxt
pNxt0, pNxt1, pNxt2, pNxt3, pNxt4, pNxt5 :: Predict
pNxt0 = [('A', "ab"), ('S', "$ab")]

pNxt1 = [('S', "$)"), ('T', "$)+-"), ('V', "$)")]

pNxt2 = [('E', "$)"), ('F', "$%)*+-/"), ('T', "$)+-"), ('U', "$)"), ('V', "$)+-")]

pNxt3 = [('A', "$ab"), ('S', "$b")]

pNxt4 = [('E', "$)+"), ('F', "$)*+"), ('T', "$)*+")]

pNxt5 = [('E', "$)+-"), ('F', "$%)*+-/"), ('T', "$%)*+-/")]

-- управляючі таблиці
ctl0, ctl1, ctl2 :: Control
ctl0 = [(('A', 'a'), 2), (('A', 'b'), 3), (('S', 'a'), 0), (('S', 'b'), 1)]

ctl1 =
  [ (('S', '('), 0)
  , (('S', 'd'), 0)
  , (('T', '('), 2)
  , (('T', 'd'), 1)
  , (('V', '$'), 5)
  , (('V', ')'), 5)
  , (('V', '+'), 3)
  , (('V', '-'), 4)
  ]

ctl2 =
  [ (('E', '('), 0)
  , (('E', 'd'), 0)
  , (('F', '('), 10)
  , (('F', 'd'), 9)
  , (('T', '('), 4)
  , (('T', 'd'), 4)
  , (('U', '$'), 1)
  , (('U', ')'), 1)
  , (('U', '+'), 2)
  , (('U', '-'), 3)
  , (('V', '$'), 5)
  , (('V', '%'), 7)
  , (('V', ')'), 5)
  , (('V', '*'), 6)
  , (('V', '+'), 5)
  , (('V', '-'), 5)
  , (('V', '/'), 8)
  ]
