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

First we build the Matrix to be transposed, i.e. we use the second function of this Assignement then we transpose

>transp a b c d = transpose (anp2 a b c d)

to do this I write my self the transposition funktion in a seperate helper for better readability and easier handling.

>transpose :: Matrix -> Matrix
>transpose ([]:_) = []

what happens here is the interesting part first we take all the first elements and put them together, than we do the same with the tails and iterate

>transpose x = (map head x) : transpose (map tail x)


So our job here is to calculate a scalar product, just to make it a little harder, we have to extract our two vectors out of matrices, and this is what we do here
 
>sp :: [[Integer]] -> [[Integer]] -> Laenge -> Fuellwert -> Integer


This is a little helper where sp actually is calcualted feed this with the correct vectors and voila

>scalprod :: [Integer] -> [Integer] -> Integer
>scalprod a b 

check for erronous input if input is ok then calculate the sp, basically take the vectors component wise multiply those and put them in to a vector, thenn sum up that vector

>    | length a == length b = sum (zipWith (*) a b)
>    | otherwise = error "Vector sizes must match"
