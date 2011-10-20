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


{-sucheAlle :: Editor ->Suchzeichenreihe -> [Index]
sucheAlle (e:es) (s:ss)
    | not (isInfixOf (s:ss) (e:es)) = []
    | length(e:es)> length(s:ss) && partialsuche = partialsuche:[ x + length (s:ss)| x <- (sucheAlle verkürzterText (s:ss))] --idea check if the first length s elements fit if yes add to list, need to add lentgh of s times number of striped beginnings
    | otherwise = []-- :[ x + length (s:ss)| x <- (sucheAlle verkürzterText (s:ss))]
    where 
        partialsuche = isInfixOf (subtext (e:es) (s:ss)) (s:ss)
        verkürzterText = stripPräfix (subtext (e:es) (s:ss)) (e:es) -- hier liegt das problem vermutlich bekomme ich aus subtext nicht immer was gutes zurück
        subtext (e:es) (s:ss) = take (length (s:ss)) (e:es)-}
        


{-ersetze :: Editor ->Vorkommen -> Alt -> Neu -> Editor
ersetze e vk a n
    | (vk-1) > vorkommenshaufigkeit || vk < 0 = e
    | otherwise = (take i e) ++ n ++ ende
    where
        vorkommenshaufigkeit = length(indizes)
        indizes = sucheAlle e a
        i = indizes !! (vk-1) CC get the vkth apperance of substring
        ende = reverse(geschnundrev)
        geschnundrev = take ((length e)-(i+length(a))) rev
        rev = reverse e
        CC idea take all the letters till the one we want to replace from (i.e. i) then concat the replacement string, then take what is after te string to be replaced
        CC to get the later reverse the string, then calculate how many to take (total length - (length of string to be replaced + length of string already taken)), take them and reverse
        CC again
-}
