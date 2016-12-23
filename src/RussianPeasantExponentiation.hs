module RussianPeasantExponentiation
    ( russianPeasantExponentiation,
      russianPeasantExponentiationCases
    ) where
  
import Numeric
import Data.Char

complexexp ::  (Integer, Integer)  -> (Integer, Integer)-> Integer -> Integer -> (Integer, Integer)
complexexp  (c, d) (e, f) k m
  | k == 1 = (c*e - d*f,c*f+d*e)
  | (k `mod` 2 == 1) = complexexp  (c,d) (  (c*e -d*f) `mod` m, (c*f + d*e) `mod`m) (k -1) m
  | otherwise = complexexp  (  (c*c - d*d) `mod` m, (2*c*d) `mod` m  ) (e,f) (k `div` 2) m

russianPeasantExponentiationCases :: Int -> IO()
russianPeasantExponentiationCases n
    | n <= 0 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let a = read $ x_t!!0 :: Integer
        let b = read $ x_t!!1 :: Integer
        let k = read $ x_t!!2 :: Integer
        let m = read $ x_t!!3 :: Integer

        let res = complexexp  (a,b) (1,0) k m
        putStrLn $ (show ((fst res) `mod` m))  ++ " "  ++  (show ((snd res) `mod` m))

        russianPeasantExponentiationCases (n-1)
        

russianPeasantExponentiation ::  IO ()
russianPeasantExponentiation = do
    n_temp <- getLine
    let n = read n_temp :: Int
    russianPeasantExponentiationCases n
    
