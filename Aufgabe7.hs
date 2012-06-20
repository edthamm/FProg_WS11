module Aufgabe7 where
import Debug.Trace

{-
############################################################

Aufgabe7.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}

--typedefs
type Vertex = Integer
type Origin = Vertex
type Destination = Vertex
type Key = Integer
type Name = Integer
type State = Integer
type StartState = State
type AcceptingStates = [State]
type Word a = [a]
type Row a = [[a]]
type Automaton a = AMgraph a


data AMgraph a = AMg [(Row a)] deriving (Eq,Show)
data BTree a = BLeaf Key a |
               BNode Key a (BTree a) (BTree a) deriving Show
data LTree a = LNode Key a [(LTree a)] deriving Show
data ALgraph = ALg [(Origin,[Destination])] deriving (Eq,Show)


--classdefs

class Structure s where
    noOfSources :: s -> Integer
    noOfSinks :: s -> Integer
    notSourceConnected :: s -> [Name]
    notSinkConnected :: s -> [Name]


-- Part 1


-- SubPart 1.1
instance Eq a => Eq (BTree a) where
    _ == _ = True
    
instance Eq a => Eq (LTree a) where
    _ == _ = True

-- SubPart 1.2
instance Structure (BTree a) where
    noOfSources _ = 1
    noOfSinks (BLeaf _ _) = 1
    noOfSinks (BNode _ _ left right) = (noOfSinks left) + (noOfSinks right)
    notSourceConnected _ = []
    notSinkConnected _ = []

instance Structure (LTree a) where
    noOfSources _ = 1
    noOfSinks (LNode _ _ []) = 1
    noOfSinks (LNode _ _ list) = sum [noOfSinks x | x <- list]
    notSourceConnected _ = []
    notSinkConnected _ = []

instance Structure ALgraph where
    noOfSources _ = 1 -- all of this is to be implemented
    noOfSinks _ = 3
    notSourceConnected _ = []
    notSinkConnected _ = []

-- helper functions

-- Part 2
accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept _ start endstates [] = _isEndState start endstates
accept (AMg automaton) start endstates word
    | word == [] = _isEndState start endstates
    | _transitionAllowed (_getRow (AMg automaton) 0 start) (head word) == True = accept (AMg automaton) (_getNewState (_getRow (AMg automaton) 0 start) (head word) 0) endstates (tail word)
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