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

-- checks if a number is primal
istPrimal :: Integer -> Bool
istPrimal n
	| (_inP n) == False = False
	| otherwise = _inList n (_sieve[2..n])

-- helper functions for istPrimal
-- sieb des eratosthenes
_sieve :: [Integer] -> [Integer]
_sieve [] = []
_sieve (x:xs) = x : _sieve [y | y <- xs, mod y x > 0]

-- checks if a number is a prime
_inList :: Integer -> [Integer] -> Bool
_inList _ [] = False
_inList n (x:xs)
	-- n == x => element ist in der liste
	| n == x = True
	| length xs == 0 = False
	| otherwise = _inList n xs

-- checks if a number is in P: (1+4n | n <- IN)
_inP :: Integer -> Bool
_inP n
	| (mod (n-1) 4) == 0 = True
	| otherwise = False

-- faktorisiert eine zahl
faktorisiere :: Integer -> [(Integer, Integer)]
faktorisiere n
	| (_inP n) == False = error "Unzulaessig"
	| otherwise = (_getFactors n (n-1))

-- helper function for faktorisiere
-- findet alle faktorisierungen einer zahl aus P die wieder in der menge P sind
_getFactors :: Integer -> Integer -> [(Integer, Integer)]
_getFactors x y
	-- verhindert divisionen durch 0 und die faktorbildung x*1
	| y == 1 = []
	-- bedingungen: 
	-- 1) nur zahlen die sich "sauber" dividieren lassen
	-- [2) das ergebnise der division muss kleiner sein als y um doppelte ergebnisse zu verhindert (x,y) == (y,x)]  && y > (div x y)
	-- 3) y muss aus der menge P sein
	-- 4) x/y muss aus der menge P sein
	-- doppelte elemente sind enthalten: zum entfernen bedingung 2 hinzufuegen
	| ((mod x y) == 0 && (_inP y) == True && (_inP (div x y)) == True) = (y, (div x y)) : (_getFactors x (y-1))
	-- y dekrementieren und erneut versuchen
	| otherwise = (_getFactors x (y-1))
	

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
