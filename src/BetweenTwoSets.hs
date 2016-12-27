module AppleAndOrange
    ( appleAndOrange
    ) where
  
import Numeric
import Data.Char
import Data.List

        

appleAndOrange ::  IO ()
appleAndOrange = do
    s_temp <- getLine
    let s_t = words s_temp
    let s = read $ s_t!!0 :: Int
    let t = read $ s_t!!1 :: Int
    a_temp <- getLine
    let a_t = words a_temp
    let a = read $ a_t!!0 :: Int
    let b = read $ a_t!!1 :: Int
    m_temp <- getLine
    let m_t = words m_temp
    let m = read $ m_t!!0 :: Int
    let n = read $ m_t!!1 :: Int
    apple_temp <- getLine
    let apple = map read $ words apple_temp :: [Int]
    orange_temp <- getLine
    let orange = map read $ words orange_temp :: [Int]

    let res1 = foldr myfunc 0 apple
               where myfunc x y = if (a + y >= s) && (a + y <= t)
                                     then x + 1
                                     else x + 0
    let res2 = foldr myfunc 0 orange
               where myfunc x y = if (b + y >= s) && (b + y <= t)
                                     then x + 1
                                     else x + 0
    print res1
    print res2
    
