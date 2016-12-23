module SherlockAndCounting
    ( sherlockAndCounting,
      sherlockAndCountingCases
    ) where
  
import Numeric
import Data.Char

complexexp :: (Integer, Integer) -> (Integer, Integer) -> Integer -> Integer -> (Integer, Integer)
complexexp (a, b) (c, d) k m
  | k == 0 = (c,d)
  | otherwise = do
      complexexp (a,b) ( (a*c - b*d), (a*d + b*c)) (k -1) m

sherlockAndCountingCases :: Int -> IO()
sherlockAndCountingCases n
    | n <= 0 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let a = read $ x_t!!0 :: Integer
        let b = read $ x_t!!1 :: Integer
        let k = read $ x_t!!2 :: Integer
        let m = read $ x_t!!3 :: Integer

        let res = complexexp (a,b) (1,0) k m
        putStrLn $ (show ((fst res) `mod` m))  ++ " "  ++  (show ((snd res) `mod` m))
        sherlockAndCountingCases (n-1)
        

sherlockAndCounting ::  IO ()
sherlockAndCounting = do
    n_temp <- getLine
    let n = read n_temp :: Int
    sherlockAndCountingCases n
    
