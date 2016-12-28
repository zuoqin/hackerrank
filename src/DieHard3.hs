module DieHard3
    ( dieHard3,
      dieHard3Cases
    ) where
  
import Numeric
import Data.Char


calccase :: Int -> Int -> Int -> Int
calccase a b c = do
      let mx = max a b
      let mn = min a b
      if (a == c || b == c)
        then 1
        else difference (if b > a then b - a else a - b) mx mn c
          where difference res mx mn c
                  | res <= 0 = 0
                  | res == c = 1
                  | otherwise = if res < mn
                                then difference (mx - (mn - res)) mx mn c
                                else difference (res - mn) mx mn c

dieHard3Cases :: Int -> IO()
dieHard3Cases cases
    | cases <= 0 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let a = read $ x_t!!0 :: Int
        let b = read $ x_t!!1 :: Int
        let c = read $ x_t!!2 :: Int
        
        let res = calccase a b c 
        putStrLn (if res == 1 then "YES" else "NO")
        dieHard3Cases (cases-1)
        

dieHard3 ::  IO ()
dieHard3 = do
    n_temp <- getLine
    let n = read n_temp :: Int
    dieHard3Cases n
    
