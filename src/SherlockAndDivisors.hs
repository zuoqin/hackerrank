module SherlockAndDivisors
    ( sherlockAndDivisors,
      sherlockAndDivisorsCases
    ) where
  
import Numeric
import Data.Char
import Data.List


calcnewe :: Int -> Int-> Int -> (Int,Int)
calcnewe e n p = if n `mod` p == 0
                    then calcnewe (e + 1) (n `div` p) p
                    else (e,n)


calccase :: Int -> Int -> Int -> Int
calccase p n ans =
  if p > n
  then ans
  else do
    let newp = if p * p > n
                  then n
                  else p
    let (e, newn) = calcnewe 0 n newp
    calccase (newp + 1) newn (ans * (e + 1))
    
  

calculateres :: Int -> Int
calculateres num
  | num `mod` 2 /= 0 = 0
  | otherwise = calccase 2 (num `div` 2) 1
      
        

sherlockAndDivisorsCases :: Int -> IO()
sherlockAndDivisorsCases tests
    | tests <= 0 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine

        let n = read $ x_temp :: Int

        
        print (calculateres n)
        sherlockAndDivisorsCases (tests-1)


  
sherlockAndDivisors :: IO ()
sherlockAndDivisors = do
    n_temp <- getLine
    let n = read n_temp :: Int
    sherlockAndDivisorsCases n

    
