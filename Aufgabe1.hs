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
    | m<r || r<0 || m<0 = (-1)
    | otherwise = binom (m,r) * fac r

numberOfOcc :: Symbol -> Text -> NumberOf
numberOfOcc _ [] = error "Program error: Result\n"
numberOfOcc a (b:bs) = 1 + numberOfOcc a bs
numberOfOcc a _ = error "Program error: kein Resultat\n"

--mostCommonSymbol :: Text -> Symbol


-- helper functions for variations

binom :: (Integer,Integer) -> Integer
binom (n,k)
    | k==0 || n==k = 1
    | otherwise = binom (n-1,k-1) + binom (n-1,k)

fac :: Integer -> Integer
fac n
    | n == 0 = 1
    | n > 0 = n * fac (n - 1)
    | otherwise = error "Nur positive Argumente!"
