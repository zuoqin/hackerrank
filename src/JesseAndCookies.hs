module JesseAndCookies
    ( jesseAndCookies
    ) where


import Data.List
import Data.PriorityQueue

calccase :: Int -> Int -> [Int] -> Int
calccase k count a
  | a !! 0 >= k = count
  | length a < 2 = -1
  | otherwise = do
    let b = drop 2 a
    let newel = (a !! 0) + (( a !! 1) * 2)
    let c = takeWhile ( < newel ) b
    let e = c ++ [newel]
    let d = dropWhile (< newel ) b
    calccase k (count + 1) (e ++ d)

jesseAndCookiesCases :: Int -> [Int] -> IO()
jesseAndCookiesCases k a
    | length a < 2 = putStrLn "-1"
    | otherwise = do
        let res = calccase k 1 (sort a)
        print res

jesseAndCookies ::  IO ()
jesseAndCookies = do
    x_temp <- getLine
    let x_t = words x_temp

    let n = read $ x_t!!0 :: Int
    let k = read $ x_t!!1 :: Int

    a_temp <- getLine
    let a = map read $ words a_temp
    jesseAndCookiesCases k a
    
