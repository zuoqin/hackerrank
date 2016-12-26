module DesignerPDFViewer
    ( designerPDFViewer
    ) where
  
import Numeric
import Data.Char
import Data.List

        

designerPDFViewer ::  IO ()
designerPDFViewer = do
    h_temp <- getLine
    let h = map read $ words h_temp :: [Int]
    word <- getLine

    let res = foldl myfunc 0 word
          where myfunc x y = max x (h !! (ord(y) - ord('a')))
    print (res * 1 * (length word))
    
