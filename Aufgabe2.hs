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


--sucheAlle :: Editor ->Suchzeichenreihe -> [Index]

--ersetze :: Editor ->Vorkommen -> Alt -> Neu -> Editor
