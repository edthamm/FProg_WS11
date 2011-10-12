{-
############################################################

Aufgabe1.hs
erstellt von:
) Matthias Krug
) Eduard Thamm 0525087

############################################################
-}


--pick :: Integer -> [Integer] -> [Integer]


--pickAll :: [Integer] -> [Integer] -> [Integer]


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



{-mostCommonSymbol :: Text -> Symbol
mostCommonSymbol (a:b:cs)

mostCommonSymbol [a,b]
    | a==b = a
    | otherwise = error "kein Resultat"
mostCommonSymbol [a] = a --this has unwanted side effects BBCCA -> A
mostCommonSymbol [] = error "Resultat"-}


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
