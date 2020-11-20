{-# OPTIONS_GHC -Wall #-}
module Xenofontova09 where

import Data.List
--import qualified Text.ParserCombinators.Parsec as P


data RE = Null      |
          Term Char |
          Seq RE RE |
          Alt RE RE |
          Rep RE    |
          Plus RE   |
          Opt RE
        deriving (Eq, Show)

type State = Int
data Label = C Char | Eps deriving (Eq, Ord, Show)
type Transition = (State, State, Label)
type Automation = (State, [State], [Transition])

type MetaState = [State]
type MetaTransition = (MetaState, MetaState, Label)


--Детермінований чи ні?
isDeter :: Automation -> Bool
isDeter (_, _, trx) =
  (not . any (\(_, _, l) -> l == Eps)) trx &&
  (not . any ((> 1) . length) . group . sort . map (\(s, _, l) -> (s, l))) trx

getStartState :: Automation -> State
getStartState (start, _, _) = start

-- Задача 1 -----------------------------------------
-- Функція simplify re, котра вилучає всі + або ? з виразу re  використовуючи правила спрощення

simplify :: RE -> RE
simplify Null = Null
simplify (Term re) = Term re
simplify (Seq re1 re2) = Seq (simplify re1) (simplify re2)
simplify (Alt re1 re2) = Alt (simplify re1) (simplify re2)
simplify (Rep re) = Rep (simplify re)
simplify (Plus re) = Seq (simplify re) (Rep (simplify re))
simplify (Opt re) = Alt (simplify re) Null
simplify re = re

-- Задача 2 -----------------------------------------
-- Предикат isTerminal aut s і предикат isEssential aut s, що повертає значення True тоді і тільки тоді, коли стан s
-- являється заключним станом або суттєвим станом, відповідно, автомату aut.
-- Стан s - суттєвий коли він або заключний або автомат aut в цьому стані можна прочитати
-- на вході деякий символ (тобто існує перехід виду (s,t,C c))
isTerminal :: Automation -> State -> Bool
isTerminal aut s = elem s (getTerminalStates aut)

isEssential :: Automation -> State -> Bool
isEssential aut s =  (containsCharC (getTransitions aut) s)  || isTerminal aut s

getTransitions:: Automation->[Transition]
getTransitions (_,_, tr) = tr

containsCharC::[Transition]->State->Bool
containsCharC [] _ = False
containsCharC ((s1, _, C _):tr) st = (s1 == st) || (containsCharC tr st)
containsCharC ((_,_, _):tr) st = containsCharC tr st

getTerminalStates :: Automation -> [State]
getTerminalStates (_, st, _) = st


-- Задача 3 -----------------------------------------
-- Функція transitionsFrom aut s, котра повертає список переходів, що виходять з стану s в автоматі aut
transitionsFrom :: Automation -> State -> [Transition]
transitionsFrom aut a = filter (\t -> checkTransition a t) (getTransitions aut)

checkTransition :: State -> Transition -> Bool
checkTransition a (b, _, _) = a == b

-- Задача 4 -----------------------------------------
-- Функція  labels trx, котра повертає всі мітки без дублікатів, що  з’являються в списку переходів trx. Довільні Eps потрібно вилучити   з результату
labels :: [Transition] -> [Label]
labels trx =  nub (map getLabel (filter (\t -> getLabel t /= Eps) trx))

getLabel :: Transition -> Label
getLabel (_, _, l) = l

-- Задача 5 ------------------------------****-----------
-- Функція  acceptsDA daut st, котра повертає  True, в тому і тільки в тому випадку коли
-- автомат детермінований daut допускає рядок st
acceptsDA :: Automation -> String -> Bool
acceptsDA = undefined


-- Задача 6 -----------------------------------------
-- Функція  stStep naut st mc, котра обчислює множину станів, в які може перейти недетермінований автомат naut
-- зі стану st за один крок, прочитавши символ ‘с’, якщо mc == C ‘c’, або по порожньому переходу, якщо mc == Eps.

-- Функція setStep naut bs mc, котра обчислює множину станів, в які може перейти недетермінований автомат naut
-- з одного із стану bs за один крок, прочитавши символ ‘с’, якщо mc == C ‘c’, або по порожньому переходу, якщо mc == Eps.

-- Функція closure naut ss, котра обчислює множину станів, в які недетермінований автомат naut
-- може перейти з довільного стану з ss, не читаючи на вході жодного символу  (лише по порожнім переходам).
stStep  :: Automation -> State -> Label -> [State]
stStep (_, _, transit) st mc = [to | (from, to, l) <- transit, (from == st), (l == mc)]


setStep :: Automation -> [State] -> Label -> [State]
setStep naut bs mc = reverse (nub (concatMap (\x -> stStep naut x mc) bs))


closure :: Automation -> [State] -> [State]
closure naut ss = nub (sort ((closureUtility ss []) ++ ss))
    where closureUtility :: [State] -> [State] -> [State]
          closureUtility [] vis = vis
          closureUtility ws vis = closureUtility res (res ++ vis)
            where res = filter (\x -> notElem x vis) (setStep naut ws Eps)

-- Задача 7 -----------------------------------------
-- Функція  accepts aut st, котра повертає  True, в тому і тільки в тому випадку коли автомат aut допускає рядок st.
-- Для реалізації можна використати функції setStep i closure
accepts :: Automation -> String -> Bool
accepts aut s = acceptsUtility aut s [getStartState aut]

acceptsUtility :: Automation -> [Char] -> [State] -> Bool
acceptsUtility _ _ [] = False
acceptsUtility aut [] st = any (\st2 -> isTerminal aut st2) st
acceptsUtility aut (x:ss) currStates = let states = closure aut currStates
                                           currState = setStep aut states (C x)
                                           in  acceptsUtility aut ss currState



-- Задача 8 ----------------------------*****-------------
-- Функція  make re beg fin nxt, котра за регулярним виразом re будує НСА з початковим станом  beg і заключним  fin
-- використовуючи при необхідності нові стани починаючи з nxt.
-- Результат функції - пара (trx,nxt1): trx – список переходів, що реалізують регулярний вираз  re, і nxt1- номер наступного стану
makeNDA :: RE -> Automation
makeNDA re = (1, [2], sort transitionsAll)
  where (transitionsAll, _) = make (simplify re) 1 2 3

make :: RE -> Int -> Int -> Int -> ([Transition], Int) 
make = undefined

-- Задача 9 --------------------------*****---------------
-- Функція  parseReg st, котра виконує синтаксичний аналіз рядка st, розпізнаючи регулярний вираз - значення типу  RE.
-- (Можна використати модуль  Parsec).
parseReg :: String -> Maybe RE 
parseReg = undefined

-- Задача 10 --------------------------*****---------------
-- Функція  makeDA nda, котра перетворює НСА nda в еквівалентний детермінований автомат.
-- Для реалізації можна визначити допоміжну функцію makeDA’, що визначена в допоміжному файлі.
makeDA' :: Automation -> (MetaState, [MetaState], [MetaTransition])
makeDA' = undefined  

makeDA :: Automation -> Automation
makeDA  = undefined
-------------------------------------------------------
-- showRE - Функція може бути корисною при тестуванні
showRE :: RE -> String
showRE (Seq re re') = showRE re ++ showRE re'
showRE (Alt re re') = "(" ++ showRE re ++ "|" ++ showRE re' ++ ")"
showRE (Rep re)     = showRE' re ++ "*"
showRE (Plus re)    = showRE' re ++ "+"
showRE (Opt re)     =  showRE' re ++ "?"
showRE re           = showRE' re

showRE' :: RE -> String
showRE' Null      = ""
showRE' (Term c)  = [c]
showRE' (Alt re re') = showRE (Alt re re')
showRE' re        = "(" ++ showRE re ++ ")"

--------------------------------------------------------
-- Тестові приклади
reFigureS, re1S, re2S, re3S, re4S, re5S, re6S :: String
reFigureS = "(a|b)*c"
re1S = "(x|y)(1|2)"
re2S = "x'*"
re3S = "(ab|c)*"
re4S = "(a?)a"
re5S = "(ab)?d+"
re6S = "c?*"

reFigure, re1, re2, re3, re4, re5, re6 :: RE
reFigure = Seq (Rep (Alt (Term 'a') (Term 'b'))) (Term 'c')
re1 = Seq (Alt (Term 'x') (Term 'y')) (Alt (Term '1') (Term '2'))
re2 = Seq (Term 'x') (Rep (Term '\''))
re3 = Rep (Alt (Seq (Term 'a') (Term 'b')) (Term 'c'))
re4 = Seq (Opt(Term 'a')) (Term 'a')
re5 = Seq (Opt (Seq (Term 'a') (Term 'b'))) (Plus (Term 'd'))
re6 = Rep (Opt (Term 'c'))

ndaFigure, nda1, nda2, nda3, nda4, nda5, nda6, ndaTest :: Automation
daFigure, da1, da2, da3, da4, da5, da6 :: Automation
ndaFigure
  = (1,[2],[(1,3,Eps),(1,5,Eps),(3,4,Eps),(4,2,C 'c'),(5,7,Eps),
            (5,9,Eps),(6,3,Eps),(6,5,Eps),(7,8,C 'a'),(8,6,Eps),
            (9,10,C 'b'),(10,6,Eps)])
daFigure
  = (1,[2],[(1,1,C 'a'),(1,1,C 'b'),(1,2,C 'c')])

nda1 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,9,Eps),(4,11,Eps),
            (5,6,C 'x'),(6,3,Eps),(7,8,C 'y'),(8,3,Eps),(9,10,C '1'),
            (10,2,Eps),(11,12,C '2'),(12,2,Eps)])
da1 = (1,[3],
     [(1,2,C 'x'),(1,2,C 'y'),(2,3,C '1'),(2,3,C '2')])

nda2 = (1,[2],[(1,3,C 'x'),(3,4,Eps),(4,2,Eps),(4,5,Eps),(5,6,C '\''),
            (6,2,Eps),(6,5,Eps)])
da2 = (1,[2],
     [(1,2,C 'x'),(2,2,C '\'')])

nda3 = (1,[2],[(1,2,Eps),(1,3,Eps),(3,5,Eps),(3,7,Eps),(4,2,Eps),
            (4,3,Eps), (5,9,C 'a'),(6,4,Eps),(7,8,C 'c'),(8,4,Eps),
            (9,10,Eps),(10,6,C 'b')])
da3 = (1,[1],
     [(1,1,C 'c'),(1,2,C 'a'),(2,1,C 'b')])

nda4 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,2,C 'a'),(5,6,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps)])
da4 = (1,[2,3],[(1,2,C 'a'),(2,3,C 'a')])

nda5 = (1,[2],[(1,5,Eps),(1,7,Eps),(3,4,Eps),(4,11,C 'd'),(5,9,C 'a'),
            (6,3,Eps),(7,8,Eps),(8,3,Eps),(9,10,Eps),(10,6,C 'b'),
            (11,12,Eps),(12,2,Eps),(12,13,Eps),(13,14,C 'd'),
            (14,2,Eps),(14,13,Eps)])
da5 = (1,[2],[(1,2,C 'd'),(1,3,C 'a'),(2,2,C 'd'),(3,4,C 'b'),
            (4,2,C 'd')])

nda6 = (1,[2], [(1,2,Eps),(1,3,Eps),(3,5,Eps),(5,6, C 'c'), (6,4,Eps), 
                (4,2,Eps), (3,7,Eps), (7,8,Eps), (8,4,Eps), (4,3,Eps)]) 
da6 = (1,[1], [(1,1, C 'c')])

ndaTest = (1, [1], [(1,2, C 'a'), (1,4, Eps), (1,3, C 'b'), (2,3, Eps),
              (3,5, Eps), (3,4, C 'a'), (4,4, Eps), (4,1, Eps), (5,2, Eps), (5,4,Eps)] )
