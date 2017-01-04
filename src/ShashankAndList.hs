module ShashankAndList
    ( shashankAndList
    ) where

powMod :: Integer -> Integer -> Integer -> Integer
powMod n exp res
  | exp > 0 = powMod ((n * n) `mod` 1000000007) (exp `div` 2) (if (exp `mod` 2) == 1 then ( (res * n) `mod` 1000000007) else res )
  | otherwise = res
  

shashankAndList ::  IO ()
shashankAndList = do
    x_temp <- getLine
    let n = read $ x_temp :: Integer

    a_temp <- getLine
    let a = map read $ words a_temp :: [Integer]

    
    let res = foldl myfunc 1 a
              where myfunc x y = (x * ((powMod 2 y 1) + 1)) `mod`  1000000007
    print (res-1)
