module SherlockAndProbability
    ( sherolockcases,
      startsherlock
    ) where

printgcd :: Int -> Int -> IO()
printgcd a b = do
  let g =  (gcd a (b*b))
  putStrLn ( (show (a `div` g) )  ++ "/" ++ (show (( b * b ) `div` g)) )
  


sherolockcases :: Int -> IO()
sherolockcases m
    | m <= 0 = putStrLn ""
    | otherwise = do
  
        x_temp <- getLine
        let x_t = words x_temp

        let n = read $ x_t!!0 :: Int
        let k = read $ x_t!!1 :: Int

        s1 <- getLine
        let s = "0" ++ s1
        -- let ones = map someones [0 .. n]
        --       where someones y = length (filter isone (take (y+1) s))
        --               where isone z = z == '1'
        let ones = map someones [0 .. n]
              where someones y
                      | y == 0 = 0
                      | otherwise = if (s !! y) == '1'
                                then (ones !! (y - 1)) + 1
                                else (ones !! (y - 1))
                                 
        --print ones
        let tot1 = map myfunc [0 .. n]
              where myfunc i = if (s !! i) == '0'
                      then 0
                      else (ones !! (min n (i + k))) - (ones !! ((max 1 (i -k)) - 1)  )
              
        let total =  foldl (+) 0 tot1

        --print total 
        if total == 0
          then print "0/1"
          else printgcd total n

        sherolockcases (m-1)

startsherlock ::  IO ()
startsherlock = do
    n_temp <- getLine
    let n = read n_temp :: Int
    sherolockcases n
