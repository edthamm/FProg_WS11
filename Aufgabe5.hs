module Aufgabe5 where

import Data.List
import Debug.Trace

{-
############################################################

Aufgabe5.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}

-- Part1
ctxlines :: [String]
ctxlines = ["hello\nhi", "a\nb\nc", "hello"]

--ctxunlines :: [String] 
--ctxwords :: [String]
--ctxunwords :: [String]
{-
all (\x->(unlines.lines)x/=x) ctxlines
(((<)2).length) ctxlines
any (((<)2).length.lines) ctxlines
-}

-- Part2
--TODO Error cases.

unixtac :: String -> String
unixtac s =unlines$reverse$lines s

unixhead :: Int -> String -> String
unixhead i s = unlines$take i $lines s

unixtail :: Int -> String -> String
unixtail i s = unlines$reverse$take i $reverse$lines s 

unixgrep :: String -> String -> String
unixgrep "" qs = unlines$lines qs
unixgrep ss qs = unlines$ [a| a <- b , isInfixOf ss a]
    where b = lines qs 

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
wordrev s = unlines$map unwords $map reverse $map words $lines s



-- Part5
unixwcw :: String -> Int
unixwcw s = length$words(s)

unixwc :: String -> (Int,Int,Int)
unixwc s = (length$lines(s) ,words, symbols)
    where words = unixwcw s
          symbols = length s
