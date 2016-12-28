module SherlockAndCounting
    ( sherlockAndCounting,
      sherlockAndCountingCases
    ) where
  
import Numeric
import Data.Char

calccase :: Int -> Int -> Int -> Int -> Int -> Int
calccase n k lo hi res
  | lo > hi  = res
  | otherwise = do
      let mid = (lo + hi) `div` 2
      if mid * ( n - mid) <= n * k
        then calccase n k (mid + 1) hi mid
        else calccase n k lo (mid -1) res

sherlockAndCountingCases :: Int -> IO()
sherlockAndCountingCases cases
    | cases <= 0 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let n = read $ x_t!!0 :: Int
        let k = read $ x_t!!1 :: Int

        let res = calccase n k 1 (n `div` 2) 0
        print (if res == 0 then res else 2 * res - (if res + res == n then 1 else 0))
        sherlockAndCountingCases (cases-1)
        

sherlockAndCounting ::  IO ()
sherlockAndCounting = do
    n_temp <- getLine
    let n = read n_temp :: Int
    sherlockAndCountingCases n
    
