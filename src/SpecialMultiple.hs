module SpecialMultiple
    ( specialMultiplecases,
      specialMultiple
    ) where
  
import Numeric
import Data.Char
import qualified Data.Map as Map

teststr :: Int -> Int -> Int
teststr nextnum num =
  do
    let s = showIntAtBase 2 intToDigit nextnum ""
    let s2 = Prelude.map (\x -> if x == '1' then '9' else x ) s
    let ln = length s2

    let sum = read s2 :: Int
    
    if sum `mod` num == 0
      then sum
      else  teststr (nextnum + 1) num


specialMultiplecases :: Int -> Map.Map Int Int -> IO()
specialMultiplecases n m
    | n <= 0 = putStrLn ""
    | otherwise = do          
        n_temp <- getLine
        let num = read n_temp :: Int

        let val = Map.lookup num $ m
        let srch = case val of
                     Just n -> n
                     Nothing -> -1

        let nextres = if srch >= 0 then srch else teststr 1 num
        let newm = if srch >= 0 then m else Map.insert num nextres $ m
        print nextres
        specialMultiplecases (n-1) newm
        

specialMultiple ::  IO ()
specialMultiple = do
    n_temp <- getLine
    let n = read n_temp :: Int
    specialMultiplecases n (Map.fromList[(0,0)])
        
