{-# OPTIONS_GHC -Wall #-}

module Xenofontova07 where
import Data.List(sort)

data BinTreeM a = EmptyM
                | NodeM a Int (BinTreeM a) (BinTreeM a)
                   deriving (Show, Eq)
-- B-������ ������� t (NodeB kl tl) =>
--      t-1 <= length kl <= 2*t-1  &&  t <= length tl <= 2*t
data Btree a =  NodeB [a] [Btree a] deriving (Show, Eq)
-- ������ �������������� B-������  (BInform heigth min max)
data BInform a = BInform {hB::Int, minB::a, maxB::a} deriving (Show, Eq)

-- ������ 1 ------------------------------------
--�������� isSearch tr,  ������ ������߹ �� ���߹���� ������ ������ � ������������  tr � ������� ������� ������ � ������������ .

rootM :: (Ord a) => BinTreeM a -> Maybe a 
rootM EmptyM = Nothing 
rootM (NodeM n k lt rt) = Just n

isSearch :: (Ord a) => BinTreeM a -> Bool
isSearch EmptyM = True
isSearch (NodeM n k lt rt) = (k>0) && maybe False (n>) (rootM lt)&& maybe False (n<) (rootM rt) 

-- ������ 2 ------------------------------------
--�������� elemSearch tr v, ������ ������߹ �� ������ ������ ������ ������ � ������������ tr �������� v
elemSearch :: (Ord a) => BinTreeM a -> a -> Bool
elemSearch EmptyM _ = False
elemSearch (NodeM n _ lt rt) v
      |n == v = True
      |v < n = elemSearch lt v
      |otherwise = elemSearch rt v

-- ������ 3 ------------------------------------
--������� insSearch  tr v, �� ������߹ � ������ ������ ������ � ������������ tr ���� �������� v.
insSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
insSearch EmptyM x = NodeM x 1 EmptyM EmptyM
insSearch (NodeM n k lt rt) x
      | x == n  = NodeM n (k+1) lt rt
      | x < n   = NodeM n k (insSearch lt x) rt
      | x > n   = NodeM n k lt (insSearch rt x)



-- ������ 4 ------------------------------------***
--������� delSearch  tr v, �� ������ � �������� ������ ������ tr  �������� v.
-- ���� ������ �� ������ ��������, �� ���������� ��� ���.
delSearch :: (Ord a) => BinTreeM a -> a -> BinTreeM a
delSearch = undefined

-- ������ 5 ------------------------------------
--������� sortList l, ����� ����� ������ l, �������������� ������ ������ ������ � ������������:
  --������� �������� ������ l, ��������� ������ ������ ������ � ������������, ��������� � ����������,
  -- (�����  ����������� ������� foldl)
sortList :: (Ord a) => [a] -> [a]
sortList xs = treeToL (foldl insSearch EmptyM xs)


treeToL::BinTreeM a -> [a]
treeToL EmptyM = []
treeToL (NodeM n k lt rt) = treeToL lt ++ replicate k n ++ treeToL rt 



-- ������ 6 ----------------------------------
-- ��������� ��� ������ tr ���� Btree a ���� �������������� � ���� ���� BInform a.
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

-- ������ 7 ----------------------------------***
-- ��������,  ������ ������߹ �� ���߹���� ��`���  tr ����  Btree  B-��-�����  ������� t
isBtree  :: (Bounded a, Ord a) => Int -> Btree a -> Bool
isBtree = undefined

-- ������ 8 ----------------------------------
-- �������� eqBtree tr1 tr2, ������ ������߹ �� ��������� ��� B-������ tr1 � tr2 �������������.
eqBtree :: (Bounded a, Ord a) => Int -> Btree a -> Btree a -> Bool
eqBtree _ tr1 tr2 = sort (treeToList  tr1) == sort (treeToList  tr2)

treeToList ::Btree a -> [a]
treeToList (NodeB [] _) = []
treeToList (NodeB n []) = n
treeToList (NodeB v (n:tr))= treeToList n ++ treeToList (NodeB v tr)


-- ������ 9 ---------------------------------
-- �������� elemBtree tr v, ������ ������߹ �� ������ B-������ tr ���� v.
elemBtree :: Ord a => Btree a -> a -> Bool
elemBtree tr n = elem n $ treeToList tr

position :: Ord a => a -> [a] -> Int
position v xs = case [ind | (ind, x) <- zip [0,1..] xs, v <= x] of
    []      -> 0
    (x:_) -> x

-- ������ 10 ----------------------------------***
-- ������� ������� �������� v � B-������ tr ������� t.
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

---------------------������ ��� - ������ ������ -------
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
