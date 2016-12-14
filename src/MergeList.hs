module MergeList
    ( mergelist
    ) where

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose 0 k = 0
choose n k = ( choose (n-1) (k-1) * n `div` k )


mergelist :: IO ()
mergelist = do
    n_temp <- getLine
    let n = read n_temp :: Int
    getMultipleLines n

getMultipleLines :: Int -> IO ()
getMultipleLines cnt
    | cnt <= 0 = putStrLn ""
    | otherwise = do
        
        x_temp <- getLine
        let x_t = words x_temp

        let m = read $ x_t!!0 :: Integer
        let n = read $ x_t!!1 :: Integer
        
        print ( (choose (n+m) m) `mod` 1000000007)
        
        getMultipleLines (cnt-1)

