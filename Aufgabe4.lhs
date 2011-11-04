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

> msk m s = scale s (anp2(m))
TODO read out m

Then we take a look at the helper function.
It takes a number and a list of lists of numbers and

> scale :: Num a => a -> [[a]] -> [[a]]
> scale = map.map.(*)

then simply maps the lists which have in turn had their elements mapped and multiplyed by s together.

mm :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung mnpw -> Matrix

ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung mnw -> Matrix

mp :: ProtoprotoMatrix -> Typung mw -> Potenz -> Matrix


TODO cut and paste anp2
