module TailorShop
    ( tailorShop
    ) where
  
import Numeric
import Data.Char
import Data.List


tailorShop ::  IO ()
tailorShop = do
    n_temp <- getLine
    let n_t = words n_temp
    let n = read $ n_t!!0 :: Int
    let p = read $ n_t!!1 :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
    let res = foldl myfunc (0,0) (sort a)
              where myfunc (x,y)  z = ( u  , y + u )
                                      where u = (max (x + 1) (if z `mod` p > 0 then (z `div` p) + 1 else (z `div` p) )   )
                                 
    print (snd res)
    
