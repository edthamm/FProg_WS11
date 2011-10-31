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


>transp :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix

First we build the Matrix to be transposed, i.e. we use the second function of this Assignement

>transp a b c d

Im just checking the first row this should be done for every row just to make sure that nobody tries messing around with underdefd matrices

>   | length a /= c || length (head a) /= b = transp (anp2 a b c d) b c d

then we transpose

>   | otherwise = transpose a 

to do this I write my self the transposition funktion in a seperate helper for better readability.

>transpose :: Matrix -> Matrix
>transpose ([]:_) = []
>transpose x = (map head x) : transpose (map tail x)

 
sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer
