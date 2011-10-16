#! runhugs
-- Testfaelle fuer Aufgabe 1 by ]af[
 
 
-- Module ----------------------------------------------------------------------
 
module Main where
 
-- Imports ---------------------------------------------------------------------
 
import Aufgabe1
import IO (stderr, hPutStr)

--added by rtbam
import qualified Control.Exception as C
 
-- Functions -------------------------------------------------------------------
 
printTest :: Bool -> IO ()
printTest False = putStr ("-----> FAILED\n")
printTest True = putStr (" PASSED\n")
 
 
test =
    do
        putStr ("Testfaelle der LVA-Leitung\n\n")
        putStr ("Aufgabe 1\r\n")
         
        printTest (pick 3 [3,2,5,3,(-5),4,3,2] == [3,3,3])
        printTest (pick (-7) [3,2,5,3,(-5),4,3,2] == [])
        printTest (pick 2 [3,2,5,3,(-5),4,3,2] == [2,2] )
        
        putStr ("Aufgabe 2\r\n")
        
        printTest (pickAll [3,2,5] [3,2,5,3,(-5),4,3,2] == [3,2,5,3,3,2])
        printTest (pickAll [(-7),5] [3,2,5,3,(-5),4,3,2] == [5])
        printTest (pickAll [] [3,2,5,3,(-5),4,3,2] == [])
        
        
        putStr ("Aufgabe 3\r\n")
        
        printTest (variations 6 3 == 120)
        printTest (variations 6 9 == (-1))
        printTest (variations 6 (-2) == (-1))
        
        putStr ("Aufgabe 4\r\n")
        
        printTest (numberOfOcc 'a' "Banane" == 2)
        printTest (numberOfOcc 'b' "Banane" == 0)
        printTest (numberOfOcc 'B' "Banane" == 1)
        
        putStr ("Aufgabe 5\r\n")
        
        printTest (mostCommonSymbol "Bananen" == 'n')
        
        
-- Testfaelle by m0ritz    
        putStr ("\nTestfaelle by m0ritz\n\n")
        putStr ("Aufgabe 1\r\n")
         
        printTest (pick (-2) [-2,2] == [-2])
        printTest (pick (-7) [3,2,5,3,(-5),4,3,2] == [])
        printTest (pick 2 [3,2,5,3,(-5),4,3,2] == [2,2])
        printTest (pick 2 [] == [])
        printTest (pick (-2) [] == [])
        
        putStr ("Aufgabe 2\r\n")
        
        printTest (pickAll [3,2,5] [3,2,5,3,(-5),4,3,2] == [3,2,5,3,3,2])
        printTest (pickAll [(-7),5] [3,2,5,3,(-5),4,3,2] == [5])
        printTest (pickAll [(-7),5] [3,2,3,(-5),4,3,2] == [])
        printTest (pickAll [3,2,5] [] == [])
        printTest (pickAll [] [3,2,5,3,(-5),4,3,2] == [])
        printTest (pickAll [] [] == [])
        printTest (pickAll [2,2,2] [1,2,2,3] == [2,2])
                
        
        putStr ("Aufgabe 3\r\n")
        
        printTest (variations 6 9 == (-1))
        printTest (variations 6 (-2) == (-1))
        printTest (variations (-2) 1 == (-1))
        printTest (variations (-2) (-2) == (-1))
        printTest (variations 0 0 == 1)
        printTest (variations 3 3 == 6)
        printTest (variations 6 3 == 120)
        
        putStr ("Aufgabe 4\r\n")
        
        printTest (numberOfOcc 'a' "Banane" == 2)
        printTest (numberOfOcc 'b' "Banane" == 0)
        printTest (numberOfOcc 'B' "Banane" == 1)
        printTest (numberOfOcc 'e' "Banane" == 1)
        printTest (numberOfOcc 'x' "Banane" == 0)
        printTest (numberOfOcc 'a' ""       == 0)
        
        putStr ("Aufgabe 5\r\n")
        
        printTest (mostCommonSymbol "Bananen" == 'n')
        printTest (mostCommonSymbol "BBB" == 'B')
        printTest (mostCommonSymbol "b" == 'b')
        printTest (mostCommonSymbol "baa" == 'a')

--added by rtbam
        printTest (mostCommonSymbol "  aa " == ' ')

        putStrLn("Aufgabe 5 - Errorfaelle")
        C.catch (putStrLn(show(mostCommonSymbol "baba")))
            (\err -> putStrLn (" \"baba\" -> " ++ show err))
        C.catch (putStrLn(show(mostCommonSymbol "")))
            (\err -> putStrLn (" \"\" -> " ++ show err))
--added by rtbam

        putStr ("\nViel Spass :) copyleft by ]af[ \r\n")
