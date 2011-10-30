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
suche _ "" = 0
suche "" _ = (-1)
suche (e:es) (s:ss)
    | not (isInfixOf (s:ss) (e:es)) = (-1)
    | take (length (s:ss)) (e:es) /= (s:ss) = (suche es (s:ss)) + 1
    | otherwise = 0


sucheAlle :: Editor ->Suchzeichenreihe -> [Index]
sucheAlle [] _ = []
sucheAlle (e:es) s
    | not (isInfixOf s (e:es)) = [] -- if its not contained return empty
    | suche subtext s /= (-1) = suche subtext s :[i + toInteger((length s))| i <- sucheAlle (verkText (length s) (e:es)) s]
    -- on a match go forward length of search string
    | suche subtext s == (-1) = [i + 1| i <- sucheAlle es s] --on a no match going forward 1 letter
        where partialSuche = suche subtext s --if I try to put this in hugs yells at me where did i format wrongly
              subtext = take (length s) (e:es)
              
--helperfunction for sucheAlle
verkText :: Int -> Editor -> Editor
verkText a (b:bs)
    | a >= length (b:bs) = []
    | a < length (b:bs) && a > 0 = verkText (a-1) bs
    | otherwise = (b:bs)


ersetze :: Editor ->Vorkommen -> Alt -> Neu -> Editor
ersetze e vk a n
    | (vk-1) > vorkommenshaufigkeit || vk < 0 = e
    | otherwise = (take (fromIntegral i) e) ++ n ++ ende
    where
        vorkommenshaufigkeit = toInteger(length(indizes)-1)
        indizes = sucheAlle e a
        i = indizes !! fromIntegral((vk-1)) -- get the vkth apperance of substring
        ende = reverse(geschnundrev)
        geschnundrev = take ((length e)-(fromIntegral(i)+length(a))) rev
        rev = reverse e
        -- idea take all the letters till the one we want to replace from (i.e. i) then concat the replacement string, then take what is after te string to be replaced
        -- to get the later reverse the string, then calculate how many to take (total length - (length of string to be replaced + length of string already taken)), take them and reverse
        -- again

