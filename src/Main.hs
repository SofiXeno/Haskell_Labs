{-# OPTIONS_GHC -Wall #-}
module Xenofontova06 where

type GraphS = (Int,[(Int,Int)])            
type Graph  = [[Int]]

-- сусіди вершини v в графі g
adj :: Graph -> Int -> [Int] 
adj g v = g !! v
-- всі вершини графа g
nodes :: Graph -> [Int]
nodes g = [0..(length g - 1)]
-- ребро належить графу
edgeIn :: Graph -> (Int, Int) -> Bool
edgeIn g (x,y) = elem y (g!!x)                  -- g!!x …. adj g x
-- всі ребра графу
edges :: Graph -> [(Int,Int)]
edges g = [(x,y) | x<-nodes g, y <- g!!x]  -- g!!x …. adj g x

                       
                       
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

-- Задача 1 ------------------------------------
--Предикат isOrdinary gr, що перевіряє чи задає список gr типа Graph неорієнтований граф
isOrdinary :: Graph -> Bool 
isOrdinary gr = False `notElem` [x `elem` gr!!y | x<-nodes gr, y<-gr!!x]

-- Задача 2 ------------------------------------
--Функція fromGraph gr, що перетворює представлення графу списком суміжності gr в стандартне.

fromGraph :: Graph -> GraphS 
fromGraph gr = (length gr-1, [(x,y) | x<-nodes gr, y<-gr!!x])

-- Задача 3 ------------------------------------
--Функція toGraph grS , що перетворює стандартне представлення графу grS в задання списком суміжності.

toGraph :: GraphS -> Graph
toGraph gr = [[s | (v, s) <- snd gr, v == x] | x <- [0,1..(fst gr)]]

-- Задача 4 ------------------------------------
--Функція shortWay gr a b,  котра знаходить в неорієнтованому gr графі найкоротший шлях, що з’єднує дві вершини графа a і b. Якщо в графі не існує шляху, що з’єднує вершини a і b, то функція повертає порожній список [].  


shortWay :: Graph -> Int -> Int -> [Int] 
shortWay gr a b = shortestWay (waysFromAToB gr a b)

waysFromAToB :: Graph -> Int ->Int -> [[Int]]
waysFromAToB gr from to = filter (\x -> to == head x) (concat (allWays gr from))

shortestWay :: [[Int]] -> [Int]
shortestWay [] = []
shortestWay l = snd (minimum [(length l1,l1) | l1 <- l])

-- Задача 5 ------------------------------------
--Предикат isConnecting gr, що перевіряє чи є неорієнтований граф gr – зв’язним.

isConnecting :: Graph -> Bool 
isConnecting = undefined

-- Задача 6 ------------------------------------
--Функція components gr , що знаходить всі зв’язні компоненти неорієнтованого графу gr.

components :: Graph -> [[Int]] 
components = undefined

-- Задача 7 ------------------------------------
--Функція eccentricity gr v, що обчислює ексцентриситету вершини v неорієнтованого зв’язного графа gr.

eccentricity :: Graph -> Int -> Int 
eccentricity gr v = maximum (map (\x -> (length (shortWay gr v x))-1) [0,1..(length gr-1)])

-- Задача 8 ------------------------------------
--Функції findDiameter gr і findRadius gr, котрі обчислюють, відповідно, діаметр і радіус неорієнтованого зв’язного графа gr. 

findDiameter :: Graph -> Int 
findDiameter gr = maximum [eccentricity gr x | x <- nodes gr]

findRadius :: Graph -> Int 
findRadius gr = minimum [eccentricity gr x | x <- nodes gr]

-- Задача 9 ------------------------------------
--Функція findCenter gr, що знаходить список вершин, котрі утворюють центр неорієнтованого зв’язного графа gr.

findCenter :: Graph -> [Int] 
findCenter gr = [ x | x<-[0,1..length gr-1], (eccentricity gr x) == (findRadius gr)]

-- Задача 10 ------------------------------------
--10.	Функція shortWays gr a b,  котра знаходить в неорієнтованому gr графі всі різні найкоротші шляхи, що з’єднують дві вершини графа a і b .

shortWays :: Graph -> Int -> Int -> [[Int]] 
shortWays gr from to = case waysFromAToB gr from to of
              [] -> []
              xs -> map (reverseList) (filter (\x-> (length (last xs)) == (length x) ) xs) 

---------------------Тестові дані - графи -------
gr1S, gr2S:: GraphS
gr1S = (5,[(0,1),(0,2),(0,3),(1,0),(1,3),(1,4),
           (2,0),(2,4),(2,5),(3,0),(3,1),(4,1),(4,2),(5,2)])
gr2S = (7,[(0,1),(0,3),(1,0),(1,2),(2,1),(2,3),(3,0),(3,2),
           (4,5),(4,6),(5,4),(5,6), (6,4),(6,5)])

gr1, gr2:: Graph
gr1 = [[1,2,3],[0,3,4],[0,4,5],[0,1],[1,2],[2]]
gr2 = [[1,3],[0,2],[1,3],[0,2],[5,6],[4,6],[4,5],[]]


