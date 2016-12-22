module IsFibo
    ( isFibo,
      isFiboCases
    ) where
  
import Numeric
import Data.Char
import Data.Set as Set

fibonaci :: Num b => [b]
fibonaci = Prelude.map fst (iterate f (0,1)) where f (x,y) = (y,x+y)

isFiboCases :: Int -> Set Int  -> IO()
isFiboCases n m
    | n <= 0 = putStrLn ""
    | otherwise = do          
        n_temp <- getLine
        let num = read n_temp :: Int

        let val = Set.member num m

        if val == True then putStrLn "IsFibo" else putStrLn "IsNotFibo"

        isFiboCases (n-1) m
        

isFibo ::  IO ()
isFibo = do
    n_temp <- getLine
    let n = read n_temp :: Int
    isFiboCases n (Set.fromList(takeWhile (<=10000000000) fibonaci))
    
