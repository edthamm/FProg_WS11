module Aufgabe6 where

import Data.List
import Debug.Trace

{-
############################################################

Aufgabe6.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}

--Typedefs to spec
data DOrd = Infix | Praefix | Postfix |
            GInfix | GPraefix | GPostfix
data BTree = Nil |
             BNode Int BTree BTree deriving (Eq,Ord,Show)
type Control = String
type Func = Integer -> Integer
type Data = Integer
data Tree = Leaf Func
          | Node Func Tree Tree Tree
data LTree = LNode Integer [LTree] deriving Show


--flatten :: BTree -> DOrd -> [Int]

--isST :: BTree -> Bool

mkControl :: String -> Control
mkControl s = [o | o <- s ,o == 'l'|| o == 'r' || o == 'm']

--apply :: Control -> Data -> Tree -> Integer

--mapLT :: Func -> LTree -> LTree
