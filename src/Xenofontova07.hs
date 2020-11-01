{-# OPTIONS_GHC -Wall #-}

module Xenofontova07 where
import Data.List(sort)

data BinTreeM a = EmptyM
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq)
-- B-дерево пор€дка t (NodeB kl tl) =>
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a] deriving (Show, Eq)
-- головні характеристики B-дерево  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- Задача 1 ------------------------------------
--Предикат isSearch tr,  котрий перевіряє чи являється бінарне дерево з повтореннями  tr – бінарним деревом пошуку з повтореннями .

rootM :: (Ord a) => BinTreeM a -> Maybe a 
rootM EmptyM = Nothing 
rootM (NodeM n k lt rt) = Just n

isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM n k lt rt) = (k>0) && maybe False (n>) (rootM lt)&& maybe False (n<) (rootM rt) 

-- Задача 2 ------------------------------------
--Предикат elemSearch tr v, котрий перевіряє чи містить бінарне дерево пошуку з повтореннями tr значення v
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM n _ lt rt) v
      |n == v = True
      |v < n = elemSearch lt v
      |otherwise = elemSearch rt v

-- Задача 3 ------------------------------------
--Функція insSearch  tr v, що вставляє в бінарне дерево пошуку з повтореннями tr нове значення v.
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM x = NodeM x 1 EmptyM EmptyM
insSearch (NodeM n k lt rt) x
      | x == n  = NodeM n (k+1) lt rt
      | x < n   = NodeM n k (insSearch lt x) rt
      | x > n   = NodeM n k lt (insSearch rt x)



-- Задача 4 ------------------------------------***
--Функція delSearch  tr v, що вилучає з бінарного дерева пошуку tr  значення v.
-- Якщо дерево не містить значення, то залишається без змін.
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch = undefined

-- Задача 5 ------------------------------------
--Функція sortList l, котра сортує список l, використовуючи бінарне дерево пошуку з повтореннями:
  --додаючи елементи списку l, формується бінарне дерево пошуку з повтореннями, починаючи з порожнього,
  -- (можна  використати функцію foldl)
sortList :: (Ord a) => [a] -> [a]
sortList xs = treeToL (foldl insSearch EmptyM xs)


treeToL::BinTreeM a -> [a]
treeToL EmptyM = []
treeToL (NodeM n k lt rt) = treeToL lt ++ replicate k n ++ treeToL rt 



-- Задача 6 ----------------------------------
-- знаходить для даного tr типу Btree a його характеристики – дане типу BInform a.
findBInform :: (Bounded a, Ord a) => Btree a ->  BInform a
findBInform tr = BInform (h_B tr) (min_B tr) (max_B tr)

min_B::Btree a ->a
min_B (NodeB n []) = head n
min_B (NodeB _ tr)= min_B(head tr)

max_B::Btree a ->a
max_B (NodeB n []) = last n
max_B (NodeB _ tr)= max_B (last tr)

h_B::Btree a ->Int
h_B (NodeB _[]) = 0
h_B (NodeB _ tr)= 1+ h_B(head tr)

-- Задача 7 ----------------------------------***
-- предикат,  котрий перевіряє чи являється об`єкт  tr типа  Btree  B-де-ревом  порядка t
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool
isBtree = undefined

-- Задача 8 ----------------------------------
-- Предикат eqBtree tr1 tr2, котрий перевіряє чи являються два B-дерева tr1 і tr2 еквівалентними.
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool
eqBtree _ tr1 tr2 = sort (treeToList  tr1) == sort (treeToList  tr2)

treeToList ::Btree a -> [a]
treeToList (NodeB [] _) = []
treeToList (NodeB n []) = n
treeToList (NodeB v (n:tr))= treeToList n ++ treeToList (NodeB v tr)


-- Задача 9 ---------------------------------
-- Предикат elemBtree tr v, котрий перевіряє чи містить B-дерево tr ключ v.
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr n = elem n $ treeToList tr

position :: Ord a => a -> [a] -> Int
position v xs = case [ind | (ind, x) <- zip [0,1..] xs, v <= x] of
    []      -> 0
    (x:_) -> x

-- Задача 10 ----------------------------------***
-- функція вставки елемента v в B-дерево tr порядка t.
insBtree :: Ord a => Int -> Btree a -> a -> Btree a
insBtree = undefined

isFull :: Ord a => Int -> Btree a -> Bool
isFull l (NodeB n _ ) = length n == 2*l-1

insertKey v [] = [v]
insertKey v (x:xs)
    | v <= x = v:(x:xs)
    | v>x = x:(insertKey v xs)

decomposeNodeB :: Ord a => a -> [a] -> [Btree a] ->
                        ([a], [a], [Btree a], Btree a, [Btree a])
decomposeNodeB = undefined

splitAtB :: Ord a => Int -> Btree a -> (Btree a, a, Btree a)
splitAtB = undefined

---------------------“естов≥ дан≥ - ƒерева пошуку -------
bm :: BinTreeM Char
bm = NodeM  't' 2
            (NodeM 'a' 1  EmptyM
                    (NodeM 'e' 1
                             (NodeM 'd' 2 EmptyM EmptyM)
                             (NodeM 'f' 1 EmptyM EmptyM)
                    )
            )
            (NodeM 'w' 2  EmptyM EmptyM)

tBt1 :: Btree Char
tBt1 = NodeB "L"
       [ NodeB "DG"
          [ NodeB "AC" [], NodeB "EE" [], NodeB "HK" []
          ]
       , NodeB "PU"
          [ NodeB "MM" [], NodeB "RS" [], NodeB "UW" []
          ]
       ]

tBt2 :: Btree Char
tBt2 = NodeB "GP"
       [ NodeB "ACDEE" [], NodeB "HKLMM" [], NodeB "RSUUW" []
       ]

tBt5 :: Btree Char
tBt5 = NodeB "GMPX"
       [ NodeB "ACDE" [] , NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt6 :: Btree Char
tBt6 = NodeB "GMPX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "RSTUV" [], NodeB "YZ" []
       ]

tBt7 :: Btree Char
tBt7 = NodeB "GMPTX"
       [ NodeB "ABCDE" [], NodeB "JK" [], NodeB "NO" []
       , NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
       ]

tBt8 :: Btree Char
tBt8 = NodeB "P"
       [ NodeB "GM"
          [ NodeB "ABCDE" [], NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX"
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]

tBt9 :: Btree Char
tBt9 = NodeB "P"
       [ NodeB "CGM"
          [ NodeB "AB" [], NodeB "DEF" []
          , NodeB "JKL" [], NodeB "NO" []
          ]
       , NodeB "TX"
          [ NodeB "QRS" [], NodeB "UV" [], NodeB "YZ" []
          ]
       ]
