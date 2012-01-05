module Aufgabe9 where


{-
############################################################

Aufgabe9.hs
erstellt von:
) Matthias Krug 0828965
) Eduard Thamm 0525087

############################################################
-}

-- Part 1

type Row = [Integer]
type Skyline = [Row]


isValidSL :: Skyline -> Bool
isValidSL _ = True


compVisibility :: Skyline -> Skyline
compVisibility a = a

--buildSkyscrapers :: Skyline -> Maybe Skyline


-- Part 2

type Sudoku = [Row]
data Variant = Basic | Cross | Color deriving (Eq,Show)

isValidSDK :: Sudoku -> Variant -> Bool
isValidSDK _ _ = True

--solve :: Sudoku -> Variant -> Maybe Sudoku
