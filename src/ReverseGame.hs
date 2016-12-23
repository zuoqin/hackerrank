module ReverseGame
    ( reverseGame,
      reverseGameCases
    ) where
  
import Numeric
import Data.Char

getIndex ::  Int -> Int -> Int
getIndex n k = if k < (n `div` 2)
                     then 2 * k + 1
                     else 2 * (n - k -1)

reverseGameCases :: Int -> IO()
reverseGameCases t
    | t < 1 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let n = read $ x_t!!0 :: Int
        let k = read $ x_t!!1 :: Int

        print( getIndex n k )

        reverseGameCases (t-1)
        

reverseGame ::  IO ()
reverseGame = do
    n_temp <- getLine
    let t = read n_temp :: Int
    reverseGameCases t
    
