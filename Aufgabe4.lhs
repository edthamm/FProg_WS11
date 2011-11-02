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

msk :: ProtoMatrix -> Skalar -> Matrix

mm :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung mnpw -> Matrix

ms :: ProtoprotoMatrix -> ProtoprotoMatrix -> Typung mnw -> Matrix

mp :: ProtoprotoMatrix -> Typung mw -> Potenz -> Matrix
