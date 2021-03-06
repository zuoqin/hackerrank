module LittleGauravAndSequence
    ( littleGauravAndSequence
    ) where

import Numeric
calc1 :: Integer -> Integer
calc1 n
    | (n `mod` 2) == 1 = 5
    | otherwise =  1

calc2 :: Int -> Integer
calc2 n =
    if n == 0
    then 2
    else do
          let new_n = n `mod` 5
          if new_n == 1
          then 6
          else if new_n == 2
               then 2
               else if new_n == 3
                    then 8
                    else if new_n == 4
                         then 4
                         else 0


decToBin x = reverse $ decToBin' x
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a
    
readdata :: Int -> IO()
readdata t
  | t > 0 = do
      x_temp  <- getLine
      let x_num = read $ x_temp :: Integer

      let lg = 63 - (64 - length (decToBin x_num))


      print( ( (calc1 x_num) * ( calc2 lg)) `mod` 10)
      readdata (t - 1)
  | otherwise = do
      putStr ""




-- main :: IO ()
-- main = do
--     q_temp <- getLine
--     let q = read q_temp :: Int
--     forM_ [1..q] $ \a0  -> do
--         x_temp <- getLine
--         let x = read x_temp :: Int
--         let arr = decToBin x
--         let l = length arr
--         let res = foldl myfunc 0 (zip arr [0..]) 
--                             where myfunc a b = a + ((if (fst b) == 0 then 1 else 0) * (2 ^ (snd b)))
--         print res

littleGauravAndSequence :: IO ()
littleGauravAndSequence = do
    x_temp <- getLine
    let n = read $ x_temp :: Int

    
    readdata n
