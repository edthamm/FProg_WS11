module Aufgabe9 where
import Data.List

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


type Count = Integer       -- buildings count
type Highest = Integer     -- the (currently) highest (found) building
type Vis = Integer         -- the visibility - don't care which site
type Skyscraperline = Row
type VisFromLeft = Integer
type VisFromRight = Integer

x = [[0,5,4,3,2,1,0],[5,10,20,30,40,50,1],[4,20,30,40,50,10,2],[3,30,40,50,10,20,2],[2,40,50,10,20,30,2],[1,50,10,20,30,40,2],[0,1,2,2,2,2,0]]

isValidSL :: Skyline -> Bool
isValidSL [] = False
isValidSL s = (_isValidSLSub horiz) && (_isValidSLSub vert)
	where
		-- remove the first and the last line
		horiz = reverse $ tail $reverse $ tail s
		-- transpose matrix & remove the first and the last line
		vert =  reverse $ tail $ reverse $ tail $ _transpose s

_isValidSLSub :: Skyline -> Bool
_isValidSLSub [] = True
_isValidSLSub (x:xs) = (isValid x) && (_isValidSLSub xs)

_transpose :: [[a]]->[[a]]
_transpose ([]:_) = []
_transpose x = (map head x) : _transpose (map tail x)

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

compVisibility :: Skyline -> Skyline
compVisibility sl = _transpose $ compVisibilitySub $ _transpose $ compVisibilitySub sl

compVisibilitySub :: Skyline -> Skyline
compVisibilitySub sl = [(computeVisibility x) | x <- sl]


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

--buildSkyscrapers :: Skyline -> Maybe Skyline


-- Part 2
type Sudoku = [Row]
data Variant = Basic | Cross | Color deriving (Eq,Show)

isValidSDK :: Sudoku -> Variant -> Bool
isValidSDK sdk Basic = (_checkRows sdk) && (_checkCols sdk) && (_checkSquares sdk)
isValidSDK sdk Cross = (isValidSDK sdk Basic) && (_checkCross sdk)
isValidSDK sdk Color = (isValidSDK sdk Basic) && (_checkColor sdk)

-- check rows
_checkRows :: Sudoku -> Bool
_checkRows [] = True
_checkRows (x:xs) = (_checkContentSDK x) && (_checkRows xs)

-- check cols
_checkCols :: Sudoku -> Bool
_checkCols sdk = _checkRows $ _transpose sdk

-- check squares
_checkSquares :: Sudoku -> Bool
_checkSquares sdk = _checkRows ([(_getSquareAsRow csdk 0 x y) | x <- [0..2], y <- [0..2]])
	where csdk = concat sdk

-- check diagonales
_checkCross :: Sudoku -> Bool
_checkCross sdk = _checkRows [(_getModXAsRow csdk 0 10 0)] && _checkRows [(_getModXAsRow csdk 0 8 0)]
	where csdk = concat sdk

-- check color fields
_checkColor :: Sudoku -> Bool
_checkColor sdk = _checkRows ([(_getModXAsRow csdk 0 9 x) | x <- [0..8]])
	where csdk = concat sdk

-- check if the "content" is right
_checkContentSDK :: Row -> Bool
_checkContentSDK line = _checkCSDK (sort line) 0

-- check a single row
_checkCSDK :: Row -> Integer -> Bool
_checkCSDK [] _ = True
_checkCSDK (x:xs) num
	| x > 9 = False
	| x == 0 = True && (_checkCSDK xs 0)
	| num == x = False
	| otherwise = True && (_checkCSDK xs x)

-- get row - defined by a mod & result value
_getModXAsRow :: Row -> Integer -> Integer -> Integer -> Row
_getModXAsRow [] _ _ _ = []
_getModXAsRow (x:xs) index m e
	| (mod index m) == e = x : (_getModXAsRow xs (index+1) m e)
	| otherwise = (_getModXAsRow xs (index+1) m e)

-- get a square transformet to a single row
_getSquareAsRow :: Row -> Integer -> Integer -> Integer -> Row
_getSquareAsRow [] _ _ _ = []
_getSquareAsRow (x:xs) index row col
	| (ncol >= (col*3)) && (ncol < (3+(col*3))) && (nrow >= (row*3)) && (nrow < (3+(row*3))) = x : (_getSquareAsRow xs (index+1) row col)
	| otherwise = (_getSquareAsRow xs (index+1) row col)
	where
		ncol = mod index 9
		nrow = (_div index 9 0)

-- division with an integer as result
_div :: Integer -> Integer -> Integer -> Integer
_div a b erg
	| a < b = erg
	| otherwise = _div (a-b) b (erg+1)

--solve :: Sudoku -> Variant -> Maybe Sudoku
