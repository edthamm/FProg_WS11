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

isPostfix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> Bool
isPostfix _ start endstates [] = _isEndState start endstates
isPostfix (AMg automaton) start endstates word
    | word == [] = _isEndState start endstates
    | _transitionAllowed (_getRow (AMg automaton) 0 start) (head word) == True = isPostfix (AMg automaton) (_getNewState (_getRow (AMg automaton) 0 start) (head word) 0) endstates (tail word)
    | otherwise = False

-- get a row from the automaton
_getRow :: Eq a => (Automaton a) -> StartState -> State -> (Row a)
_getRow (AMg (x:xs)) start state
    | start == state = x
    | otherwise = _getRow (AMg xs) (start+1) state

-- get the new state - after the transition
-- use this function only if you have checked that the transition is allowed
_getNewState :: Eq a => (Row a) -> a -> StartState -> State
_getNewState [] _ _ = error "this should never happen ..."
_getNewState (x:xs) transition state
    | (length (filter (==transition) x) /= 0) = state
    | otherwise = _getNewState xs transition (state+1)

-- check if a transition between states with a given symbol is allowed
_transitionAllowed :: Eq a => (Row a) -> a -> Bool
_transitionAllowed [] transition = False
_transitionAllowed (x:xs) transition = (length (filter (==transition) x) /= 0) || _transitionAllowed xs transition

-- check if the given state is an acceptable endstate
_isEndState :: State -> AcceptingStates -> Bool
_isEndState _ [] = False
_isEndState s (x:xs)
    | s == x = True
    | otherwise = _isEndState s xs

-- part 2
--givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a))

-- part 3
type Count = Integer       -- buildings count
type Highest = Integer     -- the (currently) highest (found) building
type Vis = Integer         -- the visibility - don't care which site

-- (a) checks if a given skycraperline is valid
isValid :: Skyscraperline -> Bool
isValid line = (_checkLeft left content) && (_checkRight right content) && (_checkContent content)
    where left = head line
          right = last line
          content = reverse $ tail $ reverse $ tail line

-- check the left view
_checkLeft :: VisFromLeft -> Skyscraperline -> Bool
_checkLeft left line = _checkSide left 0 0 line
-- check the right view
_checkRight :: VisFromRight -> Skyscraperline -> Bool
_checkRight right line = _checkSide right 0 0 (reverse line)
-- check either side (calculates from left to right)
_checkSide :: Vis -> Count -> Highest -> Skyscraperline -> Bool
_checkSide vis count _ [] = vis == count
_checkSide vis count highest (x:xs)
    | x > highest = _checkSide vis (count+1) x xs
    | otherwise = _checkSide vis count highest xs

-- check if the "content" is right: only 10, 20, 30, ..., n*10 allowed
_checkContent :: Skyscraperline -> Bool
_checkContent line = (sort line) == ([x*10 | x <- [1..(_length line)]])

-- length for integers
_length :: [a] -> Integer
_length [] =  0
_length(x:xs) = 1 + _length xs

-- (b) computates the visibility values for a scyscraper line
computeVisibility :: Skyscraperline -> Skyscraperline
computeVisibility line = [_computeLeftVisibility line] ++ line ++ [_computeRightVisibility line]

-- computate left site
_computeLeftVisibility :: Skyscraperline -> VisFromLeft
_computeLeftVisibility line = _computeVisibility 0 0 line
-- computate right site
_computeRightVisibility :: Skyscraperline -> VisFromRight
_computeRightVisibility line = _computeVisibility 0 0 (reverse line)
-- computate either side (calculates from left to right)
_computeVisibility :: Count -> Highest -> Skyscraperline -> Vis
_computeVisibility count _ [] = count
_computeVisibility count highest (x:xs)
    | x > highest = _computeVisibility (count+1) x xs
    | otherwise = _computeVisibility count highest xs

-- (c) try to build a skyscraper line which fulfills the requirements
buildSkyscrapers :: Length -> VisFromLeft -> VisFromRight -> Maybe Skyscraperline
buildSkyscrapers l left right
    | res == [] = Nothing
    | otherwise = Just (head res)
    where res = allSkyscraperLines l left right

-- (d) count all possible skyscraperlines for a given length and visibility
noOfSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> Integer
noOfSkyscraperLines l left right = _length (allSkyscraperLines l left right)

-- (e) list all possible skyscraperlines for a given length and visibility - sorted lexicographical
allSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> [Skyscraperline]
allSkyscraperLines l left right = _filterLines (_buildAllLines l) left right

-- filter all skyscraper lines which do not fulfill the requirements
_filterLines :: [Skyscraperline] -> VisFromLeft -> VisFromRight -> [Skyscraperline]
_filterLines slines left right = [x | x <- slines, ((head x) == left) && ((head $ reverse x) == right)]
-- build all lines for a given length
_buildAllLines :: Length -> [Skyscraperline]
_buildAllLines l = [computeVisibility y | y <- _permutation [x*10 | x <- [1..l]]]
-- permutate a given list
_permutation :: Skyscraperline -> [Skyscraperline]
_permutation [] = [[]]
_permutation xs = [x:ys | x <- xs, ys <- _permutation (delete x xs)]