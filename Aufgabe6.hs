module Aufgabe6 where


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

-- directly copying the tcs from pdf gives a ` instead of a ' for character delimiter
-- this breaks my hugs 

apply :: Control -> Data -> Tree -> Integer
apply c d (Leaf t) = (t d)
apply [] d (Node t _ _ _) = (t d)
apply c d (Node t v1 v2 v3)
    | (head $ mkControl c) == 'l' = apply (drop 1 c) (t d) v1
    | (head $ mkControl c) == 'm' = apply (drop 1 c) (t d) v2
    | (head $ mkControl c) == 'r' = apply (drop 1 c) (t d) v3

mapLT :: Func -> LTree -> LTree
mapLT a (LNode b []) = (LNode (a b) [])
mapLT a (LNode b c) = (LNode (a b) (appl a c))

appl :: Func -> [LTree] -> [LTree]
appl _ [] = []
appl a ((LNode b xs):ys) = (LNode (a b) (appl a xs)) : appl a ys

