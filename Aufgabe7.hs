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
instance Eq a => Eq (BTree a) where
    _ == _ = True
    
instance Eq a => Eq (LTree a) where
    _ == _ = True

-- SubPart 1.2
instance Structure (BTree a) where
    noOfSources _ = 1
    noOfSinks _ = 3 -- to be implemented
    notSourceConnected _ = []
    notSinkConnected _ = []

instance Structure (LTree a) where
    noOfSources _ = 1
    noOfSinks _ = 3 --to be implemented
    notSourceConnected _ = []
    notSinkConnected _ = []

instance Structure ALgraph where
    noOfSources _ = 1 -- all of this is to be implemented
    noOfSinks _ = 3
    notSourceConnected _ = []
    notSinkConnected _ = []

-- Part 2
accept :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Word a) -> Bool
accept _ _ _ _ = True
