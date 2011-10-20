module Aufgabe2 where

import Data.List

{-
############################################################

Aufgabe2.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}


--istPrimal :: Integer -> Bool


--faktorisiere :: Integer -> [(Integer,Integer)]

type Editor = String
type Suchzeichenreihe = String
type Index = Integer
type Vorkommen = Integer
type Alt = String
type Neu = String

suche :: Editor -> Suchzeichenreihe-> Index
suche (e:es) (s:ss)
    | not (isInfixOf (s:ss) (e:es)) = (-1)
    | take (length (s:ss)) (e:es) /= (s:ss) = (suche es (s:ss)) + 1
    | otherwise = 0

{-
sucheAlle :: Editor ->Suchzeichenreihe -> [Index]
sucheAlle (e:es) (s:ss)
    | not (isInfixOf (s:ss) (e:es)) = []
    | partialsuche /= -1 = [partialsuche]:[ x + length (s:ss)| x <- (sucheAlle (stripPräfix (subtext (e:es) (s:ss))) (s:ss))] CC idea check if the first length s elements fit if yes add to list, need to add lentgh of s times number of striped beginnings
    | otherwise = [] CC :[ x + length (s:ss)| x <- (sucheAlle (stripPräfix (subtext (e:es) (s:ss))) (s:ss))]
    where 
        partialsuche = subsuche (subtext (e:es) (s:ss))
        subsuche a b = suche a b
        subtext (e:es) (s:ss) = take (length (s:ss)) (e:es)-}


--ersetze :: Editor ->Vorkommen -> Alt -> Neu -> Editor
