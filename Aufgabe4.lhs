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

>msk :: ProtoMatrix ->Skalar ->Matrix

Ok first we normalize to call scale

>msk (m,z,s,f) sk = scale sk (anp2 m z s f) -- DO ERROR CASE

Then we take a look at the helper function.
It takes a number and a list of lists of numbers and

>scale :: Num a =>a ->[[a]] ->[[a]]
>scale = map.map.(*)

then simply maps the lists which have in turn had their elements mapped and multiplyed by s together (by the way this is done by partial appliance if I understood the concept of currying correctly [we pass around functions here]). this could also be achieved by doing a doulbe list comprahension, but 
that would be a lot harder to read

>mm :: ProtoprotoMatrix ->ProtoprotoMatrix ->Typung_mnpw ->Matrix

This is the fitting step. Normalize everything the way we are supposed to.

>mm a b (m,n,p,f) = mmult (anp2 a m n f) (anp2 b n p f)

After that we once again call a little helper

>mmult :: Matrix ->Matrix ->Matrix

multiplying 2 matrices involves multiplying row values and colums values and summing them up. since we have no easy access to colums we use transpose to get it.
we then simply zip the rows (remember we just transposed b) with *, we then sum up and map those sums in to a list, we then use list comprahension,
to get the desired list of lists.

>mmult a b = [map (sum . zipWith (*) r) $ transpose' b | r <- a]


ms :: ProtoprotoMatrix ->ProtoprotoMatrix ->Typung_mnw ->Matrix

mp :: ProtoprotoMatrix ->Typung_mw ->Potenz ->Matrix


These are the functions taken from exercise 3:

>transpose' :: Matrix ->Matrix
>transpose' ([]:_) = []
>transpose' a = (map head a) : transpose (map tail a)

>length' :: [a] -> Integer
>length' v = toInteger(length v)

>take' :: Integer -> [a] -> [a]
>take' m n = take (fromIntegral m) n

>replicate' :: Integer -> a -> [a]
>replicate' m n = replicate (fromIntegral m) n

>anp2 :: [[Integer]] -> Zeilen -> Spalten -> Fuellwert -> Matrix
>anp2 m z s f
>   | z < 0 || s < 0 = error "unzulaessig"
>   | items > z = fitcol (take' z m) s f
>   | items < z = fitcol extended s f
>   | otherwise = fitcol m s f
>   where extended = m ++ (replicate' (z - items) (replicate' s f))
>         items = length' m


>fitcol :: Matrix -> Spalten -> Fuellwert -> Matrix
>fitcol [] _ _ = []
>fitcol (z:zs) s f
>   | items > s = take' s z : fitcol zs s f
>   | items < s = (z ++ (replicate' (s - items) f)): fitcol zs s f    
>   | otherwise = z : fitcol zs s f
>   where items = length' z
