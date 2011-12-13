module Aufgabe8 where
import Data.List

{-
############################################################

Aufgabe8.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}

-- types
type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]
data AMgraph a = AMg [(Row a)] deriving (Eq,Show)
type Automaton a = AMgraph a

-- types for part 1
type Postfix a = Word a

-- types for part 2
type Prefix a = Word a

-- types for part 3
type Skyscraperline = [Integer]
type Length = Integer
type VisFromLeft = Integer
type VisFromRight = Integer

-- part 1

--isPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> Bool

-- part 2
--givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a))

-- part 3
-- (a)
isValid :: Skyscraperline -> Bool
isValid line = (_checkLeft left content) && (_checkRight right content) && (_checkContent content)
    where left = head line
          right = last line
          content = reverse $ tail $ reverse $ tail line

type Count = Integer
type Highest = Integer

-- check the left view
_checkLeft :: VisFromLeft -> Skyscraperline -> Bool
_checkLeft left line = _checkLeft_ left 0 0 line
_checkLeft_ :: VisFromLeft -> Count -> Highest -> Skyscraperline -> Bool
_checkLeft_ left count _ [] = left == count
_checkLeft_ left count highest (x:xs)
    | x > highest = _checkLeft_ left (count+1) x xs
    | otherwise = _checkLeft_ left count highest xs

-- check the right view
_checkRight :: VisFromRight -> Skyscraperline -> Bool
_checkRight right line = _checkRight_ right 0 0 (reverse line)
_checkRight_ :: VisFromRight -> Count -> Highest -> Skyscraperline -> Bool
_checkRight_ right count _ [] = right == count
_checkRight_ right count highest (x:xs)
    | x > highest = _checkRight_ right (count+1) x xs
    | otherwise = _checkRight_ right count highest xs

-- check if the "content" is right: only 10, 20, 30, ..., n*10 allowed
_checkContent :: Skyscraperline -> Bool
_checkContent line = (sort line) == ([x*10 | x <- [1..(_length line)]])

-- length for integers
_length :: [a] -> Integer
_length [] =  0
_length(x:xs) = 1 + _length xs

-- (b)
--computeVisibility :: Skyscraperline -> Skyscraperline
-- (c)
--buildSkyscrapers :: Length -> VisFromLeft -> VisFromRight -> Maybe Skyscraperline
-- (d)
--noOfSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> Integer
-- (e)
--allSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> [Skyscraperline]

_buildAllLines :: Length -> [Skyscraperline]
_buildAllLines l = _permutation [x*10 | x <- [1..l]]

_permutation :: Skyscraperline -> [Skyscraperline]
_permutation [] = [[]]
_permutation xs = [x:ys | x <- xs, ys <- _permutation (delete x xs)]