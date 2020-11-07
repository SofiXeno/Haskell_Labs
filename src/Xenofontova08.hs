{-# OPTIONS_GHC -Wall #-}
module Xenofontova08 where

data Recur = Zero | Succ | Sel Int Int
           | Super Recur [Recur]
           | Prim Recur Recur
           | Mini Recur Int
           | Name String  deriving (Show, Eq)
type System = [(String,Recur)]

-- Задача 1 ------------------------------------
-- isNumbConst [(n1,f1),…,(nk,fk)] f – перевіряє, що f - «функція-число»

isNumbConst :: System->Recur -> Bool
isNumbConst _ (Zero) = True
isNumbConst _ (Succ) = False
isNumbConst _ (Sel _ _)= False
isNumbConst _ (Super _ _)= False
isNumbConst _ (Prim _ _)= False
isNumbConst _ (Mini _ _)= False

isNumbConst sys (Name f) = let f2 = filter (\(x,_) -> x==f) sys in isNumbConst sys (snd (head f2))


-- Задача 2 ------------------------------------
-- evRank [(n1,f1),…,(nk,fk)] f – обчислює ранг (арність) функції f  

evRank :: System -> Recur -> Int
evRank _ (Succ) = 1
evRank _ (Zero) = 1
evRank _ (Sel i _) = i
evRank sys (Super _ f) = evRank sys (head f)
evRank sys (Prim _ f) = (evRank sys f) -1
evRank sys (Mini f _) = (evRank sys f) -1
evRank sys (Name f) = evRank sys (findReq sys f)

findReq::System->String->Recur
findReq [] _ = undefined
findReq (x:sys) f = if (fst x == f) then snd x
                    else findReq sys f

-- Задача 3 ------------------------------------
-- isNames [(n1,f1),…,(nk,fk)]  -  перевіряє, що всі імена визначені і вірно використовуються

isNames :: System -> Bool
isNames sys = check sys && checkN (reverse sys)

check::System->Bool
check [] = True
check (x:sys) = not (checkForDublicates sys (fst x)) && check sys


checkN::System->Bool
checkN [] = True
checkN (x:sys) = isRecur sys (snd x) && checkN sys

checkForDublicates::System->String->Bool
checkForDublicates [] _ = False
checkForDublicates (x:sys) f = if fst x == f then True
                               else checkForDublicates sys f


-- Задача 4 ------------------------------------
-- isRecur [(n1,f1),…,(nk,fk)]  f -  перевіряє, що означення f коректне

isRecur :: System -> Recur -> Bool
isRecur _ (Zero) = True
isRecur _ (Succ) = True
isRecur _ (Sel _ _)  = True
isRecur sys (Prim m n)  = isRecur sys m && isRecur sys n
isRecur sys (Mini x _)  = isRecur sys x
isRecur sys (Super a b) = isRecur sys a && and [isRecur sys x | x <- b]
isRecur sys (Name str) =  checkRecName (reverse sys) str

checkRecName::System->String->Bool
checkRecName [] _ = False
checkRecName (x:sys) f = if ((fst x == f) || checkRecName sys f) then True
                         else False

-- Задача 5 ------------------------------------
-- eval [(n1,f1),…,(nk,fk)]  f [a1,…,an] – обчислює значення примітивно-рекурсивної функції f на наборі [a1, …, an]

eval :: System -> Recur -> [Int] -> Int
eval = undefined

-- Задача 6 ------------------------------------
-- evalPart [(n1,f1),…,(nk,fk)]  f [a1,…,an] - обчислює значення частково-рекурсивної функції f на наборі [a1, …, an] 

evalPart :: System -> Recur -> [Int] -> Maybe Int
evalPart = undefined

-- Задача 7 ------------------------------------
-- parseRec str – виконує синтаксичний аналіз рядка str, розпізнаючи  систему рекурсивних функцій

parseRec :: String -> Maybe System
parseRec = undefined


---------------------“Тестові дані -  -------
syst1, syst2 :: System
syst1 = [("const0", Zero)
   , ("const0v2", Super Zero [Sel 2 1])
   , ("const0v3", Super Zero [Sel 3 1])
   , ("const1v2", Super Succ [Super Zero [Sel 2 1]])
   , ("const2", Super Succ [Super Succ [Zero]])
   , ("addition", Prim (Sel 1 1) (Super Succ [Sel 3 3 ]))
   , ("multiplication", Prim Zero (Super (Name "addition") [Sel 3 3, Sel 3 1]))
   , ("notSignum", Prim (Super Succ [Zero]) (Super Zero [Sel 2 1]))
   , ("subtract1", Prim Zero (Sel 2 1))
   , ("subtraction", Prim (Sel 1 1) (Super (Name "subtract1") [Sel 3 3]))
   , ("subtractionRev", Super (Name "subtraction") [Sel 2 2, Sel 2 1])
   , ("subtractionAbs", Super (Name "addition") [Name "subtraction", Name "subtractionRev"])
   , ("subtractionAbs3", Super (Name "subtractionAbs") [Sel 3 1, Super (Name "addition") [Sel 3 2, Sel 3 3]])
   , ("subtractionPart", Mini (Name "subtractionAbs3") 100)
   ]

syst2 = [("f1", Super Succ [Zero])
        ,("f2", Super Succ [Name "f2"])
        ]


sysStr1,sysStr2 :: String
sysStr1 = " const0 = z1; const0v2  = (z1 : s21); const0v3 = (z1:s31);\n\
          \  const1v2 = (a1 : (z1 : s21));  \n\
          \  const2= (a1:(a1:z1)); addition = [s11, (a1:s33)] ;\n\
          \  multiplication = [z1 , (addition: s33,s31)]; \n\
	  \  notSignum = [(a1:z1),(z1:s21)];\n\
	  \  subtract1 = [z1,s21]; subtraction = [s11, (subtract1:s33)];\n\
	  \  subtractionRev = (subtraction : s22, s21);\n\
          \  subtractionAbs = (addition: subtraction, subtractionRev); \n\
          \  subtractionAbs3=(subtractionAbs:s31, (addition:s32,s33))  ;\n \
          \ subtractionPart = {subtractionAbs3, 100 };"

sysStr2 = " f1 = (a1:z1); f2 = (a1, f2);"
