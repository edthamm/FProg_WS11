module Aufgabe8 where


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

-- part 2
givePrefix :: Eq a => (Automaton a) -> StartState -> AcceptingStates -> (Postfix a) -> (Maybe (Prefix a))

-- part 3
-- (a)
isValid :: Skyscraperline -> Bool
-- (b)
computeVisibility :: Skyscraperline -> Skyscraperline
-- (c)
buildSkyscrapers :: Length -> VisFromLeft -> VisFromRight -> Maybe Skyscraperline
-- (d)
noOfSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> Integer
-- (e)
allSkyscraperLines :: Length -> VisFromLeft -> VisFromRight -> [Skyscraperline]