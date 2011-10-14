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
mostCommonSymbol (x:xs)
    | containsEquals (symbolOcc (x:xs)) = error "Kein Resultat" --if the list contains equals then since symbolOcc always reduces number of occurences per letter per 1 per read 2 or more letters must have had the same number of apperances in the input specification dictates throwing an error in this case
	| numberOfOcc x (x:xs) == maximum (symbolOcc (x:xs)) = x -- there is only on maximum go on and find the fitting letter
	| numberOfOcc x (x:xs) /= maximum (symbolOcc (x:xs)) = mostCommonSymbol xs -- this is not the onw looked for search on
mostCommonSymbol [a] = a --that was easy
mostCommonSymbol [] = error "Resultat" -- see spec


-- helper functions for mostCommonSymbol


symbolOcc :: Text->[NumberOf] -- constructs a list on how often the letter on this position appears from this position onward
symbolOcc (x:xs) = (numberOfOcc x (x:xs)): symbolOcc xs
symbolOcc [] = []

containsEquals :: [NumberOf] -> Bool --checks to see if the list contains more than one entry that equals the maximum entry
containsEquals x
    | length(pick (maximum x) x )/= 1 = True
    | otherwise = False


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
