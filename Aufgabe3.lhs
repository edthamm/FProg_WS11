>module Aufgabe3 where

>import Data.List


############################################################

Aufgabe3.lhs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################

>type Matrix = [[Integer]]
>type Zeilen = Integer
>type Spalten = Integer
>type Fuellwert = Integer
>type Laenge = Integer

anp1 :: [[Integer]] -> Matrix

anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix

transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix

sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer
