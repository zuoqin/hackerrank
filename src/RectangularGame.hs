module RectangularGame
    ( rectangularGamecases,
      rectangularGame
    ) where
  


rectangularGamecases :: Int -> IO [(Integer, Integer)]
rectangularGamecases n
    | n <= 0 = return []
    | otherwise = do          
        
        x_temp <- getLine
        let x_t = words x_temp

        let x = read $ x_t!!0 :: Integer
        let y = read $ x_t!!1 :: Integer
        
        xs <- rectangularGamecases (n-1)
        let ret = (x, y):xs
        return ret

rectangularGame ::  IO ()
rectangularGame = do
    n_temp <- getLine
    let n = read n_temp :: Int
    res <- rectangularGamecases n
    let a = foldr  (\x y -> ( min (fst x) (fst y), min (snd x) (snd y) )) (res !! 0) res
                   

    print ((fst a) * (snd a))
    
