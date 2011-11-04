>module Aufgabe4 where

>import Data.List


############################################################

Aufgabe4.lhs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################

>type Matrix = [[Integer]]
>type Skalar = Integer
>type Zeilen = Integer
>type Spalten = Integer
>type SpaZei = Integer
>type Fuellwert = Integer
>type ProtoMatrix = ([[Integer]],Zeilen,Spalten,Fuellwert)
>type Typung_mnpw = (Zeilen,SpaZei,Spalten,Fuellwert)
>type Typung_mnw = (Zeilen,Spalten,Fuellwert)
>type Typung_mw = (SpaZei,Fuellwert)
>type Potenz = Integer
>type ProtoprotoMatrix = [[Integer]]

> msk :: ProtoMatrix -> Skalar -> Matrix

Ok first we normalize to call scale

> msk (m,z,s,f) s = scale s (anp2(m z s f))

Then we take a look at the helper function.
It takes a number and a list of lists of numbers and

> scale :: Num a => a -> [[a]] -> [[a]]
> scale = map.map.(*)

then simply maps the lists which have in turn had their elements mapped and multiplyed by s together.

> mm :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnpw -> Matrix

This is the fitting step. Normalize everything the way we are supposed to.

> mm a b (m,n,p,f) = mult (anp2(a m n f)) (anp2(b n p f))

After that we once again call a little helper

> mult :: Num a => Matrix -> Matrix -> Matrix

multiplying 2 matrices involves multiplying rows an colums. since we have no easy aces to colums we use transpose to get it.
we then simply zip the rows (remember we just transposed b) with *, we then sum up and map back to our original shape. 

> mult a b = [map (sum . zipWith (*) r) $ transpose b | r <- a]


ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung_mnw -> Matrix

mp :: ProtoprotoMatrix -> Typung_mw -> Potenz -> Matrix


These are the functions taken from exercise 3:

>transpose :: Matrix -> Matrix
>transpose ([]:_) = []
>transpose a = (map head a) : transpose (map tail a)


TODO cut and paste anp2
