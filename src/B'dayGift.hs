module B'dayGift
    ( b'dayGift
    ) where

import Text.Printf

foo :: Integer -> Integer -> Double
foo a b = (fromIntegral a) / (fromIntegral b)


readdata :: Integer -> Integer -> IO Integer
readdata n res
  | n > 0 = do
      x_num  <- getLine
      readdata (n - 1) (res + (read $ x_num :: Integer))
  | otherwise = do
      putStr ""
      return res
  

b'dayGift ::  IO ()
b'dayGift = do
    x_temp <- getLine
    let n = read $ x_temp :: Integer

    
    res <- readdata n  0
    printf "%.1f" ( foo res 2)