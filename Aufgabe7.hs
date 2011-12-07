module Aufgabe7 where


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


-- SubPart 1.2


-- Part 2
accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept _ _ _ _ = True
