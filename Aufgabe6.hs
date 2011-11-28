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

-- knotenwerte des gesamten baums in der angegebenen ordnung in einer liste ausgeben
flatten :: BTree -> DOrd -> [Int]
flatten Nil                    _        = []
flatten (BNode val left right) Infix    = flatten_infix    (BNode val left right)
flatten (BNode val left right) Praefix  = flatten_praefix  (BNode val left right)
flatten (BNode val left right) Postfix  = flatten_postfix  (BNode val left right)
flatten (BNode val left right) GInfix   = flatten_ginfix   (BNode val left right)
flatten (BNode val left right) GPraefix = flatten_gpraefix (BNode val left right)
flatten (BNode val left right) GPostfix = flatten_gpostfix (BNode val left right)

-- infix: left - root - right
flatten_infix :: BTree -> [Int]
flatten_infix (BNode val Nil  Nil  ) = [val]
flatten_infix (BNode val Nil  right) = [val] ++ (flatten_infix right)
flatten_infix (BNode val left Nil  ) = (flatten_infix left) ++ [val]
flatten_infix (BNode val left right) = (flatten_infix left) ++ [val] ++ (flatten_infix right)

-- mirrored infix: right - root - left
flatten_ginfix :: BTree -> [Int]
flatten_ginfix (BNode val Nil  Nil  ) = [val]
flatten_ginfix (BNode val Nil  right) = (flatten_infix right) ++ [val]
flatten_ginfix (BNode val left Nil  ) = [val] ++ (flatten_infix left)
flatten_ginfix (BNode val left right) = (flatten_infix right) ++ [val] ++ (flatten_infix left)

-- praefix: root - left - right
flatten_praefix :: BTree -> [Int]
flatten_praefix (BNode val Nil  Nil  ) = [val]
flatten_praefix (BNode val Nil  right) = [val] ++ (flatten_infix right)
flatten_praefix (BNode val left Nil  ) = [val] ++ (flatten_infix left)
flatten_praefix (BNode val left right) = [val] ++ (flatten_infix left) ++ (flatten_infix right)

-- mirrored praefix: right - left - root
flatten_gpraefix :: BTree -> [Int]
flatten_gpraefix (BNode val Nil  Nil  ) = [val]
flatten_gpraefix (BNode val Nil  right) = (flatten_infix right) ++ [val]
flatten_gpraefix (BNode val left Nil  ) = (flatten_infix left) ++ [val]
flatten_gpraefix (BNode val left right) = (flatten_infix right) ++ (flatten_infix left) ++ [val]

-- postfix: left - right - root
flatten_postfix :: BTree -> [Int]
flatten_postfix (BNode val Nil  Nil  ) = [val]
flatten_postfix (BNode val Nil  right) = (flatten_infix right) ++ [val]
flatten_postfix (BNode val left Nil  ) = (flatten_infix left) ++ [val]
flatten_postfix (BNode val left right) = (flatten_infix left) ++ (flatten_infix right) ++ [val]

-- mirrored postfix: root - right - left
flatten_gpostfix :: BTree -> [Int]
flatten_gpostfix (BNode val Nil  Nil  ) = [val]
flatten_gpostfix (BNode val Nil  right) = [val] ++ (flatten_infix right)
flatten_gpostfix (BNode val left Nil  ) = [val] ++ (flatten_infix left)
flatten_gpostfix (BNode val left right) = [val] ++ (flatten_infix right) ++ (flatten_infix left)

-- ueberprueft ob ein baum ein suchbaum ist
isST :: BTree -> Bool
isST Nil = True
isST t = _checkOrder$flatten t Infix -- infix ausgabe des baums in die hilfsfunktion schicken

-- ueberprueft ob die liste lauter zahlen enthaelt die (echt) groeszer sind als ihre vorgaenger
_checkOrder :: [Int] -> Bool
_checkOrder [] = True
_checkOrder (x:xs)
	| length xs == 0 = True
	| x < (head xs) = True && (_checkOrder xs)
	| otherwise = False

mkControl :: String -> Control
mkControl s = [o | o <- s ,o == 'l'|| o == 'r' || o == 'm']

-- directly copying the tcs from pdf gives a ` instead of a ' for character delimiter
-- this breaks my hugs 

apply :: Control -> Data -> Tree -> Integer
-- if its a leaf just apply and be happy
apply c d (Leaf t) = (t d)
-- if its a node but no control info is left apply and return
apply [] d (Node t _ _ _) = (t d)
-- check where to go next, apply the function till here (gives a "semi-open" one) desc in the correct subtree, hand down the
-- function and the shortend ctrl string
apply c d (Node t v1 v2 v3)
    | c_element == 'l' = apply (drop 1 c) (t d) v1
    | c_element == 'm' = apply (drop 1 c) (t d) v2
    | c_element == 'r' = apply (drop 1 c) (t d) v3
    where c_element = head $ mkControl c

mapLT :: Func -> LTree -> LTree
-- nowhere to go apply function and return
mapLT a (LNode b []) = (LNode (a b) [])
-- places left to go, apply function and iterate
mapLT a (LNode b c) = (LNode (a b) (appl a c))

appl :: Func -> [LTree] -> [LTree]
appl _ [] = []
appl a ((LNode b xs):ys) = (LNode (a b) (appl a xs)) : appl a ys

