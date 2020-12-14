{-# OPTIONS_GHC -Wall #-}

module XenofontovaTask where

data AbstractInteger
  = Zero
  | Succ AbstractInteger
  | Pred AbstractInteger
  deriving (Show, Eq)

type Graph = [[Int]]

data Tree23 a
  = Leaf a
  | Fork2 (Tree23 a) a (Tree23 a)
  | Fork3 (Tree23 a) a (Tree23 a) a (Tree23 a)
  | Null23 -- порожнє 2-3-дерево!!!
  deriving (Eq, Show)

-- Задача 1 --------------------------------V---------
instance Ord AbstractInteger where
  (<=) Zero Zero         = True
  (<=) (Succ x) (Succ y) = (<=) x y
  (<=) (Pred x) (Pred y) = (<=) x y
  (<=) Zero (Pred _)     = False
  (<=) (Pred _) Zero     = True
  (<=) Zero (Succ _)     = True
  (<=) (Succ _) Zero     = False
  (<=) (Succ _) (Pred _) = False
  (<=) (Pred _) (Succ _) = True

-- Задача 2 ------------------------------V----------
aiToInteger :: AbstractInteger -> Integer
aiToInteger Zero     = 0
aiToInteger (Succ n) = 1 + aiToInteger n
aiToInteger (Pred n) = (-1) + aiToInteger n

-- Задача 3 -------------------------------V----------
plusAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
plusAbs Zero Zero         = Zero
plusAbs Zero x            = x
plusAbs x Zero            = x
plusAbs (Pred x) (Pred y) = plusAbs (Pred (Pred x)) y
plusAbs (Succ x) (Succ y) = plusAbs (Succ (Succ x)) y
plusAbs (Succ x) (Pred y) = plusAbs x y
plusAbs (Pred x) (Succ y) = plusAbs x y
plusAbs x (Succ y)        = plusAbs (Succ x) y
plusAbs x (Pred y)        = plusAbs (Pred x) y

-- Задача 4 ---------------------------------V--------
timesAbs :: AbstractInteger -> AbstractInteger -> AbstractInteger
timesAbs a@(Pred _) b = timesAbs (negate a) (negate b)
timesAbs a@(Succ n) b = plusAbs b (timesAbs n b)
timesAbs _ Zero       = Zero
timesAbs Zero _       = Zero


-- Задача 5 -----------------------------------V------
fromIntegerUtil :: (Eq t, Num t) => t -> AbstractInteger
fromIntegerUtil 0 = Zero
fromIntegerUtil y = Succ (fromIntegerUtil (y - 1))

instance Num AbstractInteger where
  (+) = plusAbs
  (*) = timesAbs
  negate Zero     = Zero
  negate (Succ x) = Pred (negate x)
  negate (Pred x) = Succ (negate x)
  abs x
    | x < Zero = negate (x)
    | otherwise = x
  signum x
    | x < Zero = Pred Zero
    | x > Zero = Succ Zero
    | otherwise = Zero
  fromInteger x
    | x == 0 = Zero
    | x < 0 = negate (fromIntegerUtil (negate x))
    | otherwise = fromIntegerUtil x

-- Задача 6 ---------------------------------V--------
reverseList :: [Int] -> [Int]
reverseList xs = if null xs then []
                 else reverseList (tail xs) ++ [head xs]

findUtil :: (a -> Bool) -> [a] -> Maybe a
findUtil f [] = Nothing
findUtil f (x:xs) = if f x then Just x
                    else findUtil f xs

gamiltonWay :: Graph -> Maybe [Int]
gamiltonWay gr = maybe Nothing (\x->Just(reverseList (x ++ [head x]))) (findUtil(isGamiltonWay gr) (allWays gr))

isCycle :: Graph -> [Int] -> Bool
isCycle gr vert = elem (last vert) (gr!!(head vert))

isGamiltonWay :: Graph -> [Int] -> Bool
isGamiltonWay gr p = isCycle gr p && (length p == length gr) &&  (and ( map (\x -> elem x p) (nodes gr)))

-- Задача  7 -------------------------------V----------
isAcyclic :: Graph -> Bool
isAcyclic gr = if (findUtil(isCycle gr) (allWays gr)== Nothing) then True
               else False

allWays :: Graph -> [[Int]]
allWays gr = concat [concat (until isNull (step gr) [[[v]]]) | v <- nodes gr]

nodes :: Graph -> [Int]
nodes gr = [0,1..(length gr - 1)]

step :: Graph -> [[[Int]]] -> [[[Int]]]
step _ [] = []
step gr wss@(wsn:_) = [t:w| w@(v:vs) <- wsn, notElem v vs, t <- (gr !! v)] : wss

isNull :: [[[Int]]] -> Bool
isNull xs = null (head xs)

-- Задача 8 --------------------------------V---------
isTopolSort :: Graph -> [Int] -> Bool
isTopolSort _ [] = True
isTopolSort gr (x:xs) = (all (\x -> elem x xs) (gr!!x)) && (isTopolSort gr xs)

-- Задача 9 ---------------------------------V--------
longWay :: Graph -> Int -> Int -> Maybe [Int]
longWay gr st end =
  let arr = dfs gr st end []
  in if (null arr) then Nothing
     else Just (snd ( maximum ( map (\x -> (length x, x)) (arr))))

dfs :: Graph -> Int -> Int -> [Int] -> [[Int]]
dfs gr st end t = if (st == end) then [[st]]
                   else if (elem st t) then []
                        else foldl (\x y -> map (\c -> st : c) (dfs gr y end (st : t)) ++ x) [] (gr !! (st))

--- Задача 10 ------------------------------V----------
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs)(y:ys)
           |x == y = mySort (removeDuplicates (y : merge xs ys))
           |x < y = mySort (removeDuplicates (x : y : merge xs ys))
           |otherwise = mySort (removeDuplicates (y : x : merge xs ys) )


removeDuplicates :: [Int] -> [Int]
removeDuplicates xs = removeDuplicatesHelper xs []

removeDuplicatesHelper :: [Int] -> [Int] -> [Int]
removeDuplicatesHelper [] _ = []
removeDuplicatesHelper (x:xs) found = if (elem x found) then removeDuplicatesHelper xs found
                                      else x : removeDuplicatesHelper xs (x:found)
                      

mySort :: Ord a => [a] -> [a]
mySort []     = []
mySort (p:xs) = (mySort lesser) ++ [p] ++ (mySort greater)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

--- Задача 11 ------------------------------V----------
intToString :: Int -> Int -> String
intToString n osnova = intToStringHelper n osnova ""

intToStringHelper :: Int -> Int -> String -> String
intToStringHelper 0 _ res = show res
intToStringHelper x osnova res = if (x >= osnova) then intToStringHelper (div x osnova) osnova ((toLetter(mod x osnova))++res)
                            else (toLetter x) ++ res

toLetter :: Int -> String
toLetter x = case x of
    10 -> "a"
    11 -> "b"
    12 -> "c"
    13 -> "d"
    14 -> "e"
    15 -> "f"
    _ -> show x

--- Задача 12 ----------------------------V------------
stringToInt :: Int -> String -> Maybe Int
stringToInt n xs = stringToIntRec n xs (length xs -1) 0

stringToIntRec :: Int -> String -> Int -> Int -> Maybe Int
stringToIntRec _ [] _ r = Just r
stringToIntRec osnova (x:xs) len res = if (fromLetter x >= osnova) then Nothing
                                        else stringToIntRec osnova xs (len-1) (((fromLetter x)*osnova^len)+res)

fromLetter :: Char-> Int
fromLetter x = case x of
        'a' -> 10
        'b' -> 11
        'c' -> 12
        'd' -> 13
        'e' -> 14
        'f' -> 15
        _ -> read [x]

--- Задача 13 ----------------------------------------
genExpr :: Int -> Int -> [String]
genExpr = undefined

--- Задача 14 ----------------------------------------
genExprBracket :: Int -> Int -> [String]
genExprBracket = undefined

--- Задача 15 -------------------------------V---------
isTree23 :: (Ord a) => Tree23 a -> Bool
isTree23 Null23 = True
isTree23 (Leaf a) = True
isTree23 (Fork2 tleft a tright) = (a >= maxT tleft) && (a == minT tright)
isTree23 (Fork3 tleft a tmiddle b tright) = (a >= maxT tleft) && (a == minT tmiddle) && (b >= maxT tmiddle) && (b == minT tright)

minT (Leaf a) = a
minT (Fork2 Null23 a _) = a
minT (Fork3 Null23 a _ _ _) = a
minT (Fork2 tleft _ _) = minT tleft
minT (Fork3 tleft _ _ _ _) = minT tleft


maxT (Leaf a) = a
maxT (Fork2 _ a Null23) = a
maxT (Fork3 _ _ _ a Null23) = a
maxT (Fork2 _ _ tright) = maxT tright
maxT (Fork3 _ _ _ _ tright) = maxT tright

--- Задача 16 --------------------------------V--------

toList :: (Ord a) => Tree23 a -> [a]
toList Null23 = []
toList (Leaf a) = [a]
toList (Fork2 tleft a tright) = toList tleft ++ toList tright
toList (Fork3 tleft a tmiddle b tright) = toList tleft ++ toList tmiddle ++ toList tright

eqTree23 :: (Ord a) => Tree23 a -> Tree23 a -> Bool
eqTree23 a b = if (toList a == toList b) then True
               else False


--- Задача 17 -------------------------------V---------
elemTree23 :: (Ord a) => Tree23 a -> a -> Bool
elemTree23 (Fork3 (x) y (n) a (b)) m
  | (m == y) || (m == a) = True
  | m < y = elemTree23 (x) m
  | m > y && m < a = elemTree23 (n) m
  | m > a = elemTree23 (b) m
  | otherwise = False
elemTree23 (Fork2 (x) y (n)) m
  | m == y = True
  | m < y = elemTree23 (x) m
  | m > y = elemTree23 (n) m
  | otherwise = False
elemTree23 (Leaf x) y = x == y
elemTree23 (Null23) _ = False



--- Задача 18 ----------------------------------------
insTree23 :: (Ord a) => Tree23 a -> a -> Tree23 a
insTree23 = undefined

-- isTerminal tr = True <=> якщо сини вузла tr - листки !!
isTerminal :: (Ord a) => Tree23 a -> Bool
isTerminal (Fork2 (Leaf _) _ _)     = True
isTerminal (Fork3 (Leaf _) _ _ _ _) = True
isTerminal _                        = False

-- Результат вставки вузла в 2-3-дерево,
--   корінь якого - вузол вида Fork2 або Fork3 є об`єкт із (Tree23 a, Maybe (a, Tree23 a))
--   : (a, Nothing) - результат вставки - одне 2-3-дерево a
--   : (a, Just (w, b)) - результат вставки два 2-3-дерева a i b (w - найменше значення в b)
--  insert v tr - додає значення v в довільне дерево tr
insert :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insert v tr
  | isTerminal tr = insTerm v tr
  | otherwise = insFork v tr

-- insTerm v tr - додається значення v в дерево tr з конем - термінальний вузол
insTerm :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insTerm = undefined

-- insFork v tr - додає значення v в дерево tr з корнем - нетермінальний вузол
insFork :: (Ord a) => a -> Tree23 a -> (Tree23 a, Maybe (a, Tree23 a))
insFork = undefined

---------------------Тестові дані
---------------------- Графи -------
gr1, gr2, gr3 :: Graph
gr1 = [[1, 2, 3], [2, 3], [3, 4], [4], []]

gr2 = [[3, 4], [0, 3], [0, 1, 4], [2, 4], [1]]

gr3 = [[1], [2], [3], [1], [0, 3]]

---------------------- 2-3-дерева
tr1, tr2, tr3, tr4, tr5 :: Tree23 Int
tr1 =
  Fork2
    (Fork2 (Fork2 (Leaf 0) 1 (Leaf 1)) 2 (Fork2 (Leaf 2) 3 (Leaf 3)))
    4
    (Fork2 (Fork2 (Leaf 4) 5 (Leaf 5)) 6 (Fork2 (Leaf 6) 7 (Leaf 7)))

tr2 =
  Fork3 (Fork2 (Leaf 0) 1 (Leaf 1)) 2 (Fork3 (Leaf 2) 3 (Leaf 3) 4 (Leaf 4)) 5 (Fork3 (Leaf 5) 6 (Leaf 6) 7 (Leaf 7))

tr3 = Fork3 (Fork2 (Leaf 2) 5 (Leaf 5)) 7 (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12)) 16 (Fork2 (Leaf 16) 19 (Leaf 19))

tr4 =
  Fork3
    (Fork2 (Leaf 2) 5 (Leaf 5))
    7
    (Fork3 (Leaf 7) 8 (Leaf 8) 12 (Leaf 12))
    16
    (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19))

tr5 =
  Fork2
    (Fork2 (Fork2 (Leaf 2) 5 (Leaf 5)) 7 (Fork2 (Leaf 7) 8 (Leaf 8)))
    10
    (Fork2 (Fork2 (Leaf 10) 12 (Leaf 12)) 16 (Fork3 (Leaf 16) 18 (Leaf 18) 19 (Leaf 19)))
