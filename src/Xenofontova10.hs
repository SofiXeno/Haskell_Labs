{-# OPTIONS_GHC -Wall #-}
module Xenofontova10 where

-- розглядаємо лише цілі дані: скаляри  і масиви
--------------------------------------------------------------------
import Data.List
import Data.Maybe
type Id    = String
data Value = I Int | A [(Int, Int)]  deriving (Eq, Show)
data Op    = Add | Minus | Mul | Less | Equal | Index  deriving (Eq, Show)

data Exp = Const Int
         | Var Id
         | OpApp Op Exp Exp
         | Cond Exp Exp Exp
         | FunApp Id [Exp]
         deriving (Eq, Show)

data Stmt = Assign Id Exp
          | AssignA Id Exp Exp
          | If Exp Stmt Stmt
          | While Exp Stmt
          | Call Id [Exp]
          | Block [VarDef] [Stmt]
          deriving (Eq, Show)

data VarDef  =  Arr Id | Int Id deriving (Eq, Show)

type FunDef  =  (Id, ([VarDef], Exp))
-- функції повертають лише цілі скалярні дані, не використовують глобальні дані (чисті!!)
type ProcDef = (Id, ([VarDef], Stmt))
type Program = ([VarDef], [FunDef], [ProcDef])

type StateP  = [(Id, Value)]  -- стек даних

data Type    = At | It  deriving (Eq, Show)
type FunEnv  = [(Id,[Type])]
type ProcEnv = [(Id,[Type])]
type VarEnv  = [(Id,Type)]

-- Задача 1 ------------------------------------
-- Функція updateValue a b abs, котра в списку пар abs знаходить першу пару (a1,b1), у якої a1 == a,  
-- і замінює цю пару на пару (a,b).  Якщо такої пари немає, то додає пару (a,b) в список.    
updateValue :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
updateValue a b xs = case findIndex (\(al, _) -> al == a) xs of
  Nothing -> xs ++ [(a, b)]
  Just i -> let (l, r) = splitAt i xs in l ++ [(a, b)] ++ tail r

-- Задача 2 ------------------------------------
-- Функція updateArray a i v, що повертає масив такий самий як a за винятком його 
-- елементу,  значення якого потрібно зв’язати з v.  
updateArray :: Value -> Value -> Value -> Value
updateArray (A a) (I i) (I v) = A (updateValue i v a)
updateArray _ _ _ = error "Invalid type(s) of arguments for updateArray"

-- Задача 3 ------------------------------------
--Функція  applyOp op v1 v2, котра застосовує оператор op до аргументів v1 і  v2. 
-- Для op =  Index («індексування» масиву) результат повинен бути нуль (представляється I 0), 
-- якщо немає зв’язування для індексу v2 в масиві v1
applyOp :: Op -> Value -> Value -> Value
applyOp Add (I x) (I y) = I (x+y)
applyOp Minus (I x) (I y) = I (x-y)
applyOp Mul (I x) (I y) = I (x*y)
applyOp Less (I x) (I y) = I (if x < y then 1 else 0)
applyOp Equal (I x) (I y) = I (if x==y then 1 else 0)
applyOp Index (A xs) (I i) = maybe (I 0) (\(_, r) -> I r) (find (\(a, _) -> a == i) xs)
applyOp _ _ _ = error "Invalid type(s) of arguments for applyOp"

-- Задача 4 ------------------------------------
-- Взаємно рекурсивні функції  evExp і  evArgst для обчислення виразів. 
-- Функція evExp e dfx st  обчислює значення вираз e для списку функцій визначених користувачем dfx  і стану st.  
-- evArgs ex dfx st  застосовує функцію evExp  до кожного елементу  списку виразів ex, повертаючи список значень. Функція evExp e dfx st  використовує правила:
--•	Значення константи (ціле число) (Const c) є (I c).
--•	Значення змінної отримується з стану st (стан – це стек, що містить список пар (Id,Value) ).
--•	Значення умовного  виразу - значення одного з  підвиразів, в залежності від значення першого (умовного) виразу, значення якого (I v) – False (v == 0) або – True (v/= 0).
--•	Значення бінарної операції – результат applyOp на обчислених аргументах.
--•	Щоб обчислити  застосування функції  f до списку виразів es потрібно
--o	 Знайти означення  f в списку dfx
--o	 Виділити з означення список імен аргументів as і вираз ef - тіло функції.
--o	Використовуючи evArgs, обчислити кожний вираз в es, отримуючи список значень vs.
--o	Зв’язати імена з as зі значеннями з vs, утворюючи новий стан new.
--o	Обрахувати вираз ef в стані new.   

evExp ::  Exp -> [FunDef] -> StateP -> Value
evExp (Const x) _ _ = I x
evExp (Var iden) _ st = lookUp iden st
evExp (Cond cond o1 o2) dfx st = evExp res dfx st where res = if evExp cond dfx st /= I 0 then o1 else o2
evExp (OpApp op x y) dfx st = applyOp op (evExp x dfx st) (evExp y dfx st)
evExp (FunApp iden es) dfx st = let (as, ef) = lookUp iden dfx
                                    vs = evArgs es dfx st
                                    new = zip (map takeId as) vs
                                in evExp ef dfx new

evArgs :: [Exp] -> [FunDef] ->StateP -> [Value]
evArgs es dfx st = map (\e -> evExp e dfx st) es

takeId :: VarDef -> Id
takeId (Arr str) = str
takeId (Int str) = str

-- Задача 5 ------------------------------------
--Рекурсивна функція evStmt  s dfx dpx st, котра виконує один оператор s. 
--Результат виконання  – це перетворення вхідного стану у вихідний стан. 
-- Параметри функцій –  список функцій визначених користувачем dfx, список процедур визначених користувачем dpx і вхідний стан st.
evStmt :: Stmt -> [FunDef] -> [ProcDef] -> StateP -> StateP
evStmt (Assign iden e) dfx _ st = updateValue iden (evExp e dfx st) st
evStmt (AssignA iden e1 e2) dfx _ st = let r1 = evExp e1 dfx st
                                           r2 = evExp e2 dfx st
                                           arr = lookUp iden st
                                       in updateValue iden (updateArray arr r1 r2) st
evStmt (If e o1 o2) dfx dpx st = evStmt r dfx dpx st where r = if evExp e dfx st /= I 0 then o1 else o2
evStmt (While ex stmt) dfx dpx st = until (\st' -> evExp ex dfx st' == I 0) (evStmt stmt dfx dpx ) st
evStmt (Call iden es) dfx dpx st = let (varDefs, stmt) = lookUp iden dpx
                                       ids = map takeId varDefs
                                       vals = zip ids (map (\x -> evExp x dfx st) es)
                                       new = st ++ vals
                                       newStmt = evStmt stmt dfx dpx new
                                       in filter (\(x, _) -> x `notElem` ids) newStmt
evStmt (Block defs stmts) dfx dpx st = let ids = map fst vals
                                           vals = map initv defs
                                           new = st++vals
                                           newStmt = foldl (\a x-> evStmt x dfx dpx a) new stmts  -- recStmtEv stmts dfx dpx nSt
                                           in filter (\(x, _) -> x `notElem` ids) newStmt

-- Задача 6 ------------------------------------
--	Функція iswfExp e ve fe, котра перевіряє контекстні умови виразу s,
--	використову-ючи типи доступних змінних (середовище змінних ve) і типи визначених функцій (середовище функцій fe),  
--	і повертає тип результату виразу Just t, або Nothing.  
iswfExp :: Exp -> VarEnv -> FunEnv -> Maybe Type
iswfExp (Const _) _ _ = Just It
iswfExp (Var iden) ve _ = case find (\(i, _) -> i == iden) ve of
  Nothing -> Nothing
  Just (_, x) -> Just x
iswfExp (OpApp op e1 e2) ve fe = isJustWfOp op [iswfExp e1 ve fe, iswfExp e2 ve fe]
iswfExp (Cond c e1 e2) ve fe = case [iswfExp c ve fe, iswfExp e1 ve fe, iswfExp e2 ve fe] of
  [Just It, Just It, Just It] -> Just It
  [Just It, Just At, Just At] -> Just At
  _ -> Nothing
iswfExp (FunApp iden es) ve fe = if map Just (lookUp iden fe) == map (\x -> iswfExp x ve fe) es
                                  then Just It else Nothing

isJustWfOp :: Op -> [Maybe Type] -> Maybe Type
isJustWfOp Add   [Just It,Just It] = Just It
isJustWfOp Minus [Just It,Just It] = Just It
isJustWfOp Mul   [Just It,Just It] = Just It
isJustWfOp Less  [Just It,Just It] = Just It
isJustWfOp Equal [Just It,Just It] = Just It
isJustWfOp Index [Just At,Just It] = Just It
isJustWfOp _ _      = Nothing

-- Задача 7 ------------------------------------
-- Предикат iswfStmt s ve fe pe, що перевіряє контекстні умови оператору s в середовищі доступних змінних ve, функцій fe і процедур pe. 
iswfStmt :: Stmt -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfStmt (Assign iden ex) ve fe _ = (iswfExp ex ve fe == Just It) && isJust (lookup iden ve)
iswfStmt (AssignA iden e1 e2) ve fe _ =  iswfAssignA  [lookup iden ve, iswfExp e1 ve fe, iswfExp e2 ve fe]
iswfStmt (If ex s1 s2) ve fe pe = iswfExp ex ve fe == Just It && iswfStmt s1 ve fe pe && iswfStmt s2 ve fe pe
iswfStmt (While cond s1) ve fe pe = iswfExp cond ve fe == Just It && iswfStmt s1 ve fe pe
iswfStmt (Call iden es) ve fe pe =  map Just (lookUp iden pe) == map (\x -> iswfExp x ve fe) es 
iswfStmt (Block defs sts) ve fe pe = all (\x -> iswfStmt x (ve++map unpackVarDef defs) fe pe) sts 

unpackVarDef :: VarDef -> (Id, Type)
unpackVarDef (Arr x) = (x, At)
unpackVarDef (Int x) = (x, It)
-- Задача 8 ------------------------------------
-- Предикати iswfFunDef df fe  і  iswfProcDef dp ve fe pe, 
-- шо перевіряють коректність визначення функції df і процедури dp, використовуючи середовища змінних ve, функцій fe і процедур pe.  
iswfFunDef :: FunDef -> FunEnv -> Bool
iswfFunDef = undefined

iswfProcDef :: ProcDef -> VarEnv -> FunEnv -> ProcEnv -> Bool
iswfProcDef = undefined

-- Задача 9 ------------------------------------
-- Предикат iswfProgram pr, котрий перевіряє контекстні умови програми, формуючи середовища змінних, функцій і процедур.  
iswfProgram :: Program -> Bool
iswfProgram = undefined

--- Допоміжні функції -----------------------------
lookUp :: Eq a => a -> [(a,b)] -> b
-- Передумова: Пара з ключом a є в списку пар abx
lookUp a abx = maybe (error "lookUp") id (lookup a abx)

-- формує початкове значення змінної
initv :: VarDef -> (Id, Value)
initv (Arr v) = (v, A [])
initv (Int v) = (v, I 0)

-- Реалізація виконання програми
evProgram :: Program -> StateP
evProgram (dvx, dfx, dpx) =
   let sb = map initv dvx
       ( _, s) = lookUp "main" dpx
   in  evStmt s dfx dpx sb

--  iswfOp o ts - перевіряє коректність типів операндів ts
--     бінарної операції o і формує тип результату Just t або Nothing
iswfOp :: Op -> [Type] -> Maybe Type
iswfOp Add   [It,It] = Just It
iswfOp Minus [It,It] = Just It
iswfOp Mul   [It,It] = Just It
iswfOp Less  [It,It] = Just It
iswfOp Equal [It,It] = Just It
iswfOp Index [At,It] = Just It
iswfOp _      _      = Nothing

--  iswfCond ts - перевіряє коректність  типів операндів ts
--     умовного виразу і формує тип результату Just t або Nothing
iswfCond :: [Type] -> Maybe Type
iswfCond [It,It,It] = Just It
iswfCond [It,At,At] = Just At
iswfCond _          = Nothing

-- iswfAssignA ts перевіряє коректність  типів операндів ts
--   операції присвоювання значення елементу масива
iswfAssignA :: [Maybe Type] -> Bool
iswfAssignA [Just At, Just It, Just It] = True
iswfAssignA _          = False

---- Дані для тестування  -----------------------
-- Стан для тестування
sampleState :: StateP
sampleState = [("x",I 5),("y",I 2),("a", A [(2,3),(0,4), (1,2)])]

varEnv :: VarEnv
varEnv = [("x",It), ("y",It), ("a",At)]

-- Функція максимум двох чисел
-- func biggest(m,n)= (m<n ? n : m)
biggest :: FunDef
biggest =("biggest",
          ([Int "m", Int "n"],
           Cond (OpApp  Less (Var "m") (Var "n"))  (Var "n")  (Var "m")
           )
         )
-- Функція, що обчислює число Фібоначчі
-- func fib(n) = (n<3 ? 1 : fib(n-1) + fib(n-2))
fib :: FunDef
fib = ("fib",
       ([Int "n"],
        Cond (OpApp Less (Var "n") (Const 3))
             (Const 1)
             (OpApp Add (FunApp "fib" [OpApp Minus (Var "n") (Const 1)])
                        (FunApp "fib" [OpApp Minus (Var "n") (Const 2)]))
       )
      )

-- Функція - сума елементів масиву 0..n ...
-- func sumA(a[],n) = (n<0 ? 0 : a[n] + sumA (a,n-1))
sumA :: FunDef
sumA = ("sumA",
        ([Arr "a", Int "n"],
         Cond (OpApp Less (Var "n") (Const 0))
              (Const 0)
              (OpApp Add (OpApp Index (Var "a") (Var "n"))
                         (FunApp "sumA" [Var "a", OpApp Minus (Var "n")(Const 1)])
              )
        )
       )

funEnv :: FunEnv
funEnv = [("biggest",[It,It]),("fib", [It]),("sumA",[At,It])]

-- Приклад оператору - блоку
sampleBlock :: Stmt
sampleBlock = Block [Arr "b"]
                 [AssignA "b" (Const 0) (Const 9), AssignA "b" (Const 2) (Const 5),
                  AssignA "b" (Const 3) (Const 7), AssignA "b" (Const 5) (Const 1),
                  Call "sumA1" [Var "b", Const 5]
                 ]

-- Процедура - додавання двох чисел...
-- proc gAdd(x,y) gSum = x + y
gAdd :: ProcDef
gAdd = ("gAdd",
        ([Int "x", Int "y"],
         Assign "gSum" (OpApp Add (Var "x") (Var "y"))
        )
       )

-- Процедура - сума елементів масиву 0..n ...
-- proc sumA1(a[],n) {i;limit;
--      sA=0; i=0; limit=n+1;
--      while (i<limit){sA=sA+a[i]; i=i+1}
--                   }
sumA1 :: ProcDef
sumA1 = ("sumA1",
         ([Arr "a", Int "n"],
          Block [Int "i", Int "limit"]
            [Assign "sA" (Const 0), Assign "i" (Const 0),
             Assign "limit" (OpApp Add (Var "n") (Const 1)),
             While (OpApp Less (Var "i") (Var "limit"))
                   (Block []
                     [Assign "sA" (OpApp Add (Var "sA")
                                  (OpApp Index (Var "a") (Var "i"))),
                      Assign "i" (OpApp Add (Var "i") (Const 1))
                     ]
                   )
            ]
         )
        )

procEnv :: ProcEnv
procEnv = [("gAdd",[It,It]),("sumA1",[At,It])]

-- Повні програми
-- gSum;
-- proc gAdd(x,y) gSum = x + y
-- proc main() call gAdd(5,10)
pr1 :: Program
pr1 = ([Int "gSum"], [], [gAdd, ("main",([],Call "gAdd" [Const  5, Const 10]))])

-- sA
-- proc sumA1(a[],n) {i;limit; .... }
-- proc main() {b[]; b[0]=9; b[2]=5; b[3]=7; b[5]=1;
--                   call sumA1 (b,5)
--             }

pr2 :: Program
pr2 = ([Int "sA"], [],
       [sumA1,
        ("main",([], sampleBlock))
       ])
