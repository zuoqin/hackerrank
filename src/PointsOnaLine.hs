module PointsOnaLine
    ( pointonaline,
      getMultipleLines
    ) where


checkList :: (Int, Int) -> (Int,Int) -> (Int,Int) -> Bool
checkList p1 p2 p3 = if ( (snd p2 - snd p1) * (fst p3  - fst p1) == ( snd p3 - snd p1) * (fst p2 - fst p1) && ((snd p3) == (snd p2) || (fst p3 == fst p2))  )
                         then True
                         else False


toboolean :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> Bool
toboolean lst p1 p2 = foldl (&&) True (toboolean1 lst p1 p2)


toboolean1 :: [(Int,Int)] -> (Int,Int) -> (Int,Int) -> [Bool]
toboolean1 lst p1 p2 = map isLine lst
  where isLine y = checkList p1 p2 y

pointonaline :: IO ()
pointonaline = do
    n_temp <- getLine
    let n = read n_temp :: Int
    res <- getMultipleLines n


    
    let t = (tail (tail res) )
    let p1 = (head res)

    let p2 = (head (tail res))

    let y = if n > 2
            then toboolean t p1 p2
            else if (fst p1 == fst p2) || (snd p1 == snd p2) then True else False
    
    if y == True
      then putStrLn "YES" 
      else putStrLn "NO"

getMultipleLines :: Int -> IO [(Int, Int)]
getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        
        x_temp <- getLine
        let x_t = words x_temp

        let x = read $ x_t!!0 :: Int
        let y = read $ x_t!!1 :: Int
        
        xs <- getMultipleLines (n-1)
        let ret = (x, y):xs
        return ret
