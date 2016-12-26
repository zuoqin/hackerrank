module MiniMaxSum
    ( miniMaxSum
    ) where
  
import Numeric
import Data.Char
import Data.List

        

miniMaxSum ::  IO ()
miniMaxSum = do
    x_temp <- getLine
    let x_t = words x_temp

    let n_t = map (\x -> read $ x :: Int) x_t
    let sort_n = sort n_t
    putStrLn (  (show ( foldl (+) 0 (take 4 sort_n) )) ++ " "  ++ (show ( foldl (+) 0 (drop 1 sort_n) )))
    
