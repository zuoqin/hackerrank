module ClosestNumber
    ( closestNumber,
      closestNumberCases
    ) where
  
import Numeric
import Data.Char

check :: Int -> Int
check x
    | x == 1 = 1
    | otherwise = 0

calccase :: Int -> Int -> Int -> Int
calccase a b x
    | ((a == 1) || (b == 0)) = check x
    | b < 0 = 0
    | otherwise = do
        let ab = ( a ^ b)
        if ( x == a ) || ( x == ab )
        then ab
        else do
          let md = ab `mod` x
          if md > ( x `div` 2 )
          then ab + (x - md)
          else ab - md

closestNumberCases :: Int -> IO()
closestNumberCases cases
    | cases <= 0 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let a = read $ x_t!!0 :: Int
        let b = read $ x_t!!1 :: Int
        let x = read $ x_t!!2 :: Int
        
        print( calccase a b x )

        closestNumberCases (cases-1)
        

closestNumber ::  IO ()
closestNumber = do
    n_temp <- getLine
    let n = read n_temp :: Int
    closestNumberCases n
    
