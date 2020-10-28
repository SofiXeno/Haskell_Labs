{-# OPTIONS_GHC -Wall #-}
module Xenofontova03 where


type Algorithm    = [Substitution]
type Substitution = (String,String,Bool)
type ConfigA      = (Bool, Int, String)

data Command = Z Int | S Int | T Int Int | J Int Int Int deriving (Show, Eq)
type Program = [Command]
type ConfigC = (Int, Int, [Int])

-- Задача 1 ------------------------------------
isPrefix :: String -> String -> Bool
isPrefix bs xs
  | length xs < length bs = False
  | null bs = True
  | head bs == head xs = isPrefix (tail bs) (tail xs)
  | otherwise = False

-- Задача 2 ------------------------------------

first :: (a0, a1, a2) -> a0
first (x, _, _) = x

second :: (a0, a1, a2) -> a1
second (_, x, _) = x


third :: (a0, a1, a2) -> a2
third (_, _, x) = x


substitute :: Substitution -> Int -> String -> String
substitute sub i w = fst (splitAt i w) ++ second sub ++ snd (splitAt (length (first sub)) (snd (splitAt i w)))


-- Задача 3------------------------------------
findPosition :: String -> Substitution -> [(Substitution,Int)]
findPosition w sub = if null (first sub) then [(sub, x) | x <- [0, 1..(length w)]]
                      else findPosAux w sub 0 []
                     
                     
findPosAux :: String -> Substitution -> Int -> [(Substitution, Int)] -> [(Substitution, Int)]
findPosAux w sub i res  
 | null w = res
 | isPrefix (first sub) w = findPosAux (tail w) sub (i+1) (res ++ [(sub, i)])
 | otherwise = findPosAux (tail w) sub (i+1) res 
                          

-- Задача 4 ------------------------------------
findAll :: Algorithm -> String -> [(Substitution,Int)]
findAll algo w = concatMap (findPosition w) algo

-- Задача 5 ------------------------------------
stepA :: Algorithm -> ConfigA -> ConfigA
stepA algo (bt,st,word) = do
 let sub =  head $ (findAll algo word)
 (not (third (fst sub)), (st+1), substitute (fst sub) (snd sub) word)


-- Задача 6 ------------------------------------
evalA :: Algorithm -> Int -> String -> Maybe String
evalA algo m word = evalAAux algo m (True,0,word)

evalAAux::Algorithm -> Int -> ConfigA -> Maybe String
evalAAux algo m (bt, st, w)
  | m==st = Nothing 
  | not bt = Just w
  | otherwise = evalAAux algo m (stepA algo (bt, st, w))
  

-- Задача 7 ------------------------------------

takeFromProgram::Command-> Int
takeFromProgram (Z x) = x
takeFromProgram (S x) = x
takeFromProgram (T x y) = max x y
takeFromProgram (J x y z) = max x y


maximReg :: Program -> Int
maximReg pr = maximum (map takeFromProgram pr)

-- Задача 8 ------------------------------------
ini:: Program -> [Int] -> [Int]
ini pr ir= ir ++ take (maximReg pr - length ir) [0, 0..]

upd :: [Int] -> Int -> Int-> [Int]
upd reg r v = if null reg || (r<0) then []
              else fst (splitAt r reg) ++ [v] ++ tail (snd (splitAt r reg))

-- Задача 9 ------------------------------------
stepC :: Program -> ConfigC -> ConfigC
stepC pr (nm,st,rg) = stepCAux (pr !! (nm-1)) (nm, st, rg)

stepCAux :: Command-> ConfigC -> ConfigC
stepCAux (Z x) (nm,st,reg) = (nm+1, st+1, upd reg (x-1) 0 )
stepCAux (S x) (nm,st,reg)= (nm+1, st+1, upd reg (x-1) ((reg !! (x-1))+1))
stepCAux (T x y) (nm,st,reg)= (nm+1, st+1, upd reg y (reg !! x))
stepCAux (J x y z) (nm,st,reg)= if x==y then (z,st+1,reg)
                else (nm+1, st+1, reg)


-- Задача 10 ------------------------------------
evalC :: Program -> Int -> [Int] -> Maybe Int
evalC pr mx ir = undefined

---------------------Тестові дані - Нормальні алгоритми Маркова ---------------------------
clearBeginOne, addEnd, reverse, multiply:: Algorithm
-- стирає перший символ вхідного слова (алфавіт {a,b})
clearBeginOne = [ ("ca", "", True)
                , ("cb", "", True)
                , ("", "c", False)
                ]

-- дописує abb в кінець вхідного слова (алфавіт {a,b})
addEnd = [ ("ca", "ac", False)
         , ("cb", "bc", False)
         , ("c", "abb", True)
         , ("", "c", False)
         ]
-- зеркальне відображення вхідного слова (алфавіт {a,b})
reverse = [ ("cc", "d", False)
          , ("dc", "d", False)
          , ("da", "ad", False)
          , ("db", "bd", False)
          , ("d", "", True)
          , ("caa", "aca", False)
          , ("cab", "bca", False)
          , ("cba", "acb", False)
          , ("cbb", "bcb", False)
          , ("", "c", False)
          ]

-- добуток натуральних чисел
--  multiply ("|||#||") = "||||||"  3*2 = 6
multiply = [("a|", "|ba", False)
            ,("a", "", False)
            ,("b|", "|b", False)
            ,("|#", "#a", False)
            ,("#", "c", False)
            ,("c|", "c", False)
            ,("cb", "|c", False)
            ,("c", "", True)
            ]

---------------------Тестові дані - Програми МНР ---------------------------
notSignum, addition, subtraction :: Program
-- функція notSignum x
notSignum = [Z 2, J 1 2 5, Z 1, J 1 1 6, S 1]

-- функція додавання  addition x y = x+y
addition = [Z 3, J 3 2 6, S 1, S 3, J 1 1 2]

-- функція віднімання subtraction x y = x-y, визначена для x>=y
subtraction = [Z 3, J 1 2 6, S 2, S 3, J 1 1 2, T 3 1]


