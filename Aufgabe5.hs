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
unixtac :: String -> String
unixhead :: Int -> String -> String
unixtail :: Int -> String -> String
unixgrep :: String -> String -> String

-- Part3
aslines :: ([String]->[String]) -> String -> String
unixtac' :: String -> String
unixhead' :: Int -> String -> String
unixtail' :: Int -> String -> String
unixgrep' :: String -> String -> String

-- Part4
unixrev :: String -> String
wordrev :: String -> String

-- Part5
unixwcw :: String -> Int
unixwc :: String -> (Int,Int,Int)
