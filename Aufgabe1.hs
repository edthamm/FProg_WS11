module Aufgabe1 where

import List
import Data.Ord(comparing)
import Data.List
import Debug.Trace
{-
############################################################

Aufgabe1.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}



pick :: Integer -> [Integer] -> [Integer]
pick n []       = []
pick n (x:xs)
	| n == x    = x : pick n xs -- if equal "print" number
	| otherwise = pick n xs -- recursive call

 
pickAll :: [Integer] -> [Integer] -> [Integer]
pickAll [] _      = [] -- empty input => empty output
pickAll _ []      = [] -- empty input => empty output
pickAll (xs) (ys) = [z | z <- ys, _exists z xs]	
	
-- helper function for pickAll
-- checks if an integer exists in the given list
_exists :: Integer -> [Integer] -> Bool
_exists n [] = False
_exists n (x:xs)
	| n == x = True
	| otherwise = _exists n xs


-- Typedefs as taken form specification
type Symbol = Char
type Text = String
type NumberOf = Integer

variations :: Integer -> Integer -> Integer
variations m r
    | m<r || r<0 || m<0 = (-1) -- check for "error" cases 
    | otherwise = binom (m,r) * fac r -- if none of the error cases are true use the mathematicl formula


numberOfOcc :: Symbol -> Text -> NumberOf
numberOfOcc a (b:bs) 
    | a==b = 1+(numberOfOcc a bs) --add one and call recursion
    | a/=b = 0+(numberOfOcc a bs) -- on unequal dont add anything and recurse
numberOfOcc a [] = 0 --this is needed for structure recognition the recursion must know what to do with the empty word



mostCommonSymbol :: Text -> Symbol
mostCommonSymbol [] = error "Resultat" -- see spec
mostCommonSymbol [a] = a --that was easy
mostCommonSymbol l
    | length reversed == 1 = head(head(reversed))
    | length first == length second = error "Kein Resultat"

 --idea sort the input, then group it in to lists (at this point we have a list of lists), sort this ascending(did not find a desc. sort), reverse it, if the first 2 elems are of the same length error
    | otherwise = head(head(reversed)) --if not return the first elem of the first elem
        where
            first = head reversed
	    second = head (tail reversed)
	    reversed = reverse listsorted
	    listsorted = lsort grouped
	    grouped = group sorted 
	    sorted = sort l 


-- helper functions for mostCommonSymbol

lsort :: [[a]] -> [[a]]
lsort = sortBy (comparing length)




-- helper functions for variations


binom :: (Integer,Integer) -> Integer --one needs binomial coefficient to calculate the number of variations this code was discussed in the lecture
binom (n,k)
    | k==0 || n==k = 1
    | otherwise = binom (n-1,k-1) + binom (n-1,k)

fac :: Integer -> Integer -- one needs the factorial function to calculate the number of variations this cado was also discussed in the lecture
fac n
    | n == 0 = 1
    | n > 0 = n * fac (n - 1)
    | otherwise = error "Nur positive Argumente!"
