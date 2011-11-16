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

ctxunlines :: [[String]] 
ctxunlines = [["hallo","hi\n"], ["a\n","b","c"], ["hal\nlo"]]

ctxwords :: [String]
ctxwords = ["hello hi ", " a b c", "hel  lo"," "]

ctxunwords :: [[String]]
ctxunwords = [["hallo","hi\n"], ["a\n","b","c"], ["hal\nlo"],["a    b"]]

-- Part2

-- Hängt Zeilen in umgekehrter Richtung aneinander
unixtac :: String -> String
unixtac s 
    | (isSuffixOf "\n" s) = unlines$reverse$lines s
    | otherwise = _replaceFirstNewLine $unlines$reverse$lines s -- Sonderfall falls der String am Ende kein \n enthält

-- erstes Vorkommen von \n entfernen
_replaceFirstNewLine :: String -> String
_replaceFirstNewLine "" = ""
_replaceFirstNewLine (s:xs)
    | [s] == "\n" = xs
    | otherwise = s : (_replaceFirstNewLine xs)

-- Liefert die ersten n Zeilen eines Strings
unixhead :: Int -> String -> String
unixhead i s
    | (i == (length $lines s)) && (isSuffixOf "\n" s) == False = reverse$tail$reverse$unlines$take i $lines s
    | otherwise = unlines$take i $lines s

-- Liefert die letzten n Zeilen eines Strings
unixtail :: Int -> String -> String
unixtail i s
    | (isSuffixOf "\n" s) == False = reverse$tail$reverse$unlines$reverse$take i $reverse$lines s
    | otherwise = unlines$reverse$take i $reverse$lines s

-- Liefert alle Zeilen des Strings die einen bestimmten Substring enthalten
unixgrep :: String -> String -> String
unixgrep "" qs = unlines$lines qs -- grep gibt jedes gefundene mit newline am ende aus
unixgrep ss qs = unlines$ [a| a <- b , isInfixOf ss a]
    where b = lines qs 

-- Part3

-- das Gemeinsame aus den Funktionen von Part2 herausgehoben
aslines :: ([String]->[String]) -> String -> String
aslines f s = unlines$f $lines s

-- wie in Part2 nur mit aslines
unixtac' :: String -> String
unixtac' s
    | (isSuffixOf "\n" s) = aslines reverse s
    | otherwise = _replaceFirstNewLine $aslines reverse s

		-- wie in Part2 nur mit aslines
unixhead' :: Int -> String -> String
unixhead' i s
    | (i == (length $lines s)) && (isSuffixOf "\n" s) == False = reverse$tail$reverse$aslines (take i) s
    | otherwise = aslines (take i) s

		-- wie in Part2 nur mit aslines
unixtail' :: Int -> String -> String
unixtail' i s 
    | (isSuffixOf "\n" s) == True = aslines (drop((length(lines s)) - i)) s
    | otherwise = reverse$tail$reverse$aslines (drop((length(lines s)) - i)) s

		-- wie in Part2 nur mit aslines
unixgrep' :: String -> String -> String
unixgrep' "" qs = aslines (\x->x) qs -- grep gibt jedes gefundene mit newline am ende aus 
unixgrep' ss qs = aslines (filter (isInfixOf ss)) qs
 

-- Part4
-- einen String zeilenweise invertieren
unixrev :: String -> String
unixrev s = unlines$map reverse (lines(s))

-- einen String wortweise invertieren
wordrev :: String -> String
wordrev s = unlines$map unwords $map reverse $map words $lines s



-- Part5
-- Anzahl der Woerter eines Strings ausgeben
unixwcw :: String -> Int
unixwcw s = length$words(s)

-- Anzahl der Zeilen, Woerter und Zeichen eines Strings ausgeben
unixwc :: String -> (Int,Int,Int)
unixwc s = (length$lines(s) ,words, symbols)
    where words = unixwcw s
          symbols = length s
