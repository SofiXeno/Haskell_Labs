{-# OPTIONS_GHC -Wall #-}
module Xenofontova06 where

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- ����� ������� v � ����� g
adj :: Graph -> Int -> [Int] 
adj g v = g !! v
-- �� ������� ����� g
nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]
-- ����� �������� �����
edgeIn :: Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)                  -- g!!x �. adj g x
-- �� ����� �����
edges :: Graph -> [(Int,Int)]
edges g = [(x,y) | x<-nodes g, y <- g!!x]  -- g!!x �. adj g x

                       
                       
allWays :: Graph -> Int -> [[[Int]]]
allWays gr v = until condW (stepW gr) [[[v]]]

condW :: ([[[Int]]]) -> Bool
condW wss = null ( head wss)   

stepW :: Graph -> [[[Int]]] -> [[[Int]]]
stepW gr wss@(wsn:_) =[t:w | w@(x:xs) <- wsn, notElem x xs, t<- gr!!x] : wss
stepW gr []  = []
                       
reverseList :: [Int] -> [Int]
reverseList xs =
  if null xs
    then []
    else reverseList (tail xs) ++ [head xs]

-- ������ 1 ------------------------------------
--�������� isOrdinary gr, �� ������߹ �� ���� ������ gr ���� Graph ������������ ����
isOrdinary :: Graph -> Bool 
isOrdinary gr = False `notElem` [x `elem` gr!!y | x<-nodes gr, y<-gr!!x]

-- ������ 2 ------------------------------------
--������� fromGraph gr, �� ���������� ������������� ����� ������� �������� gr � ����������.

fromGraph :: Graph -> GraphS 
fromGraph gr = (length gr-1, [(x,y) | x<-nodes gr, y<-gr!!x])

-- ������ 3 ------------------------------------
--������� toGraph grS , �� ���������� ���������� ������������� ����� grS � ������� ������� ��������.

toGraph :: GraphS -> Graph
toGraph gr = [[s | (v, s) <- snd gr, v == x] | x <- [0,1..(fst gr)]]

-- ������ 4 ------------------------------------
--������� shortWay gr a b,  ����� ��������� � ������������� gr ����� ����������� ����, �� �չ��� �� ������� ����� a � b. ���� � ����� �� ���� �����, �� �չ��� ������� a � b, �� ������� ������� ������� ������ [].  


shortWay :: Graph -> Int -> Int -> [Int] 
shortWay gr a b = shortestWay (waysFromAToB gr a b)

waysFromAToB :: Graph -> Int ->Int -> [[Int]]
waysFromAToB gr from to = filter (\x -> to == head x) (concat (allWays gr from))

shortestWay :: [[Int]] -> [Int]
shortestWay [] = []
shortestWay l = snd (minimum [(length l1,l1) | l1 <- l])

-- ������ 5 ------------------------------------
--�������� isConnecting gr, �� ������߹ �� � ������������ ���� gr � ��������.

isConnecting :: Graph -> Bool 
isConnecting = undefined

-- ������ 6 ------------------------------------
--������� components gr , �� ��������� �� ������ ���������� ������������� ����� gr.

components :: Graph -> [[Int]] 
components = undefined

-- ������ 7 ------------------------------------
--������� eccentricity gr v, �� �������� ��������������� ������� v ������������� ��������� ����� gr.

eccentricity :: Graph -> Int -> Int 
eccentricity gr v = maximum (map (\x -> (length (shortWay gr v x))-1) [0,1..(length gr-1)])

-- ������ 8 ------------------------------------
--������� findDiameter gr � findRadius gr, ���� ����������, ��������, ������ � ����� ������������� ��������� ����� gr. 

findDiameter :: Graph -> Int 
findDiameter gr = maximum [eccentricity gr x | x <- nodes gr]

findRadius :: Graph -> Int 
findRadius gr = minimum [eccentricity gr x | x <- nodes gr]

-- ������ 9 ------------------------------------
--������� findCenter gr, �� ��������� ������ ������, ���� ��������� ����� ������������� ��������� ����� gr.

findCenter :: Graph -> [Int] 
findCenter gr = [ x | x<-[0,1..length gr-1], (eccentricity gr x) == (findRadius gr)]

-- ������ 10 ------------------------------------
--10.	������� shortWays gr a b,  ����� ��������� � ������������� gr ����� �� ��� ���������� �����, �� �չ������ �� ������� ����� a � b .

shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr from to = case waysFromAToB gr from to of
              [] -> []
              xs -> map (reverseList) (filter (\x-> (length (last xs)) == (length x) ) xs) 

---------------------������ ��� - ����� -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]


