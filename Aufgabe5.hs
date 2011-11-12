module Aufgabe5 where

import Data.List

{-
############################################################

Aufgabe5.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}

-- Part1
{-
all (\x->(unlines.lines)x/=x) ctxlines
(((<)2).length) ctxlines
any (((<)2).length.lines) ctxlines
-}

-- Part2
{-unixtac :: String -> String
unixhead :: Int -> String -> String
unixtail :: Int -> String -> String
unixgrep :: String -> String -> String
-}

-- Part3
{-
aslines :: ([String]->[String]) -> String -> String
unixtac' :: String -> String
unixhead' :: Int -> String -> String
unixtail' :: Int -> String -> String
unixgrep' :: String -> String -> String
-}

-- Part4
unixrev :: String -> String
unixrev s = unlines$map reverse (lines(s))

wordrev :: String -> String
wordrev s = unwords$map reverse (words(s))
-- Part5
unixwcw :: String -> Int
unixwcw s = length$words(s)

unixwc :: String -> (Int,Int,Int)
unixwc s = (length$lines(s) ,words, symbols)
    where words = unixwcw s
          symbols = length s
