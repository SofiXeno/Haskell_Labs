{-# OPTIONS_GHC -Wall #-}
module Xenofontova04 where


import Data.Char(isDigit, digitToInt)

-- Задача 1 -----------------------------------------

analyseG :: String -> Bool
analyseG st = case s st of
  Just st1 -> null st1
  Nothing  -> False

s :: String -> Maybe String
s ('a' : st1) = case s st1 of
  Just ('b' : st2) -> case a st2 of
    Just ('a' : st3) -> Just st3
    _                -> Nothing
  _                -> Nothing
s ('b' : st1) = Just st1
s _ = Nothing

a :: String -> Maybe String
a ('b' :('a' : st1)) = case a st1 of
   Just st2 -> s st2
   _        -> Nothing
a ('a' : st1) = Just st1
a _ = Nothing

-- Задача 2 ----------------------------------------

balance :: String -> Bool
balance st = case b st of
  Just st1 -> null st1
  Nothing  -> False

b :: String -> Maybe String
b st = e (c st)

c :: String -> String
c (' ':st1) = c st1
c st        = st

e :: String -> Maybe String
e ('(':st1) = case b st1 of
          Just (')':st2) -> b st2
          _                -> Nothing
e ('[':st1) = case b st1 of
          Just (']':st2) -> b st2
          _                -> Nothing
e ('{':st1) = case b st1 of
          Just ('}':st2) -> b st2
          _                -> Nothing
e st        = Just st

-- Задача 3 -----------------------------------------

analyseExpr :: String -> Bool
analyseExpr st = case ae st of
  Just st1 -> null st1
  Nothing  -> False

ae :: String -> Maybe String
ae st = case af st of
  Just st1 -> aa st1
  _        -> Nothing

aa :: String -> Maybe String
aa (t : st) | elem t "+*-" = ae st
aa st = Just st

af :: String -> Maybe String
af ('(' : st) = case ae st of
  Just (')' : st1) -> Just st1
  _                -> Nothing
af (t : st) | isDigit t = Just st
af _                    = Nothing

-- Задача 4 -----------------------------------------

evalLeft :: String -> Maybe Int
evalLeft st = case le st of
  Just (x, st1) | null st1 -> Just x
  _                      -> Nothing

le :: String -> Maybe (Int, String)
le st = case lf st of
          Just st1 -> la st1
          Nothing  -> Nothing

la :: (Int, String) -> Maybe (Int,String)
la (x1, t : st1) | elem t "*+-" = case lf st1 of
   Just (x2, st2) -> la (inOp t x1 x2, st2)
   _              -> Nothing
la (x, st) = Just (x, st)

lf :: String -> Maybe (Int, String)
lf ('(' : st1) = case le st1 of
  Just (x ,')' : st2) -> Just (x, st2)
  _                   -> Nothing
lf (t : st) | isDigit t = Just (digitToInt t, st)
lf _                    = Nothing

-- Задача 5 -----------------------------------------

evalRigth :: String -> Maybe Int
evalRigth st = case re st of
  Just (x, st1) | null st1 -> Just x
  _                      -> Nothing

re :: String -> Maybe (Int, String)
re st = case rf st of
  Just st1 -> ra st1
  Nothing  -> Nothing

ra :: (Int,String) -> Maybe (Int,String)
ra (x1, t : st1) | elem t "*+-" = case re st1 of
      Just (x2, st2) -> ra (inOp t x1 x2, st2)
      _              -> Nothing
ra (x, st) = Just (x, st)

rf :: String -> Maybe (Int, String)
rf ('(' : st1) = case re st1 of
  Just (x ,')' : st2) -> Just (x, st2)
  _                   -> Nothing
rf (t : st) | isDigit t = Just (digitToInt t, st)
rf _                    = Nothing

-- Задача 6 -----------------------------------------

evalPrior :: String -> Maybe Int
evalPrior st = case pe st of
  Just (x, st1) | null st1 -> Just x
  _                      -> Nothing

pe :: String -> Maybe (Int, String)
pe st = case pt st of
          Just st1 -> pa st1
          Nothing  -> Nothing

pa :: (Int,String) -> Maybe (Int,String)
pa (x1, t : st1) | elem t "+-" = case pt st1 of
   Just (x2, st2) -> pa (inOp t x1 x2, st2)
   _              -> Nothing
pa (x, st) = Just (x, st)

pt :: String -> Maybe (Int, String)
pt st = case pf st of
          Just st1 -> pb st1
          Nothing  -> Nothing

pb :: (Int,String) -> Maybe (Int,String)
pb (x1, '*' : st1) = case pf st1 of
   Just (x2, st2) -> pb (inOp '*' x1 x2, st2)
   _              -> Nothing
pb (x, st) = Just (x, st)

pf :: String -> Maybe (Int, String)
pf ('(' : st1) = case pe st1 of
  Just (x ,')' : st2) -> Just (x, st2)
  _                   -> Nothing
pf (t : st) | isDigit t = Just (digitToInt t, st)
pf _                    = Nothing

------------------------------------------------------
match :: Char -> Maybe String -> Maybe String
match c1 (Just (t:st)) | c1==t = Just st
match _ _                      = Nothing

inOp :: Char -> Int -> Int -> Int
inOp c2 = case c2 of {'+' -> (+); '-' -> (-); '*' -> (*); _ -> undefined}