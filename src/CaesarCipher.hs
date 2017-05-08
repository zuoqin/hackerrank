module CaesarCipher
    ( caesarCipher
    ) where

import Data.Char

movelow :: Char -> Int -> Char
movelow c k =
  if ord(c) + k > 122 then chr( 96 + ord(c) + k - 122 ) else chr( ord(c) + k )

moveup :: Char -> Int -> Char
moveup c k =
  if ord(c) + k > 90 then chr( 64 + ord(c) + k - 90 ) else chr( ord(c) + k )


processdata :: [Char] -> Int -> IO()
processdata s k
  | k >= 0 = do
      let res = map (\x -> if (ord x) <= 122 && (ord x) >= 97 then movelow x k else if (ord x) <= 90 && (ord x) >= 65 then moveup x k else x) s
      putStrLn res
  | otherwise = do
      putStrLn s


caesarCipher :: IO ()
caesarCipher = do
    n_temp <- getLine
    let n = read n_temp :: Int
    s <- getLine
    k_temp <- getLine
    let k = read k_temp :: Int
    
    processdata s (rem k 26)
    
