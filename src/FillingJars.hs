module FillingJars
    ( getlines,
      startjars
    ) where



getlines :: Int -> IO Integer
getlines m
    | m <= 0 = do return 0
    | otherwise = do
  
        x_temp <- getLine
        let x_t = words x_temp

        let a = read $ x_t!!0 :: Integer
        let b = read $ x_t!!1 :: Integer
        let k = read $ x_t!!2 :: Integer

        total <-  getlines (m-1)
        let ret = total + (b - a + 1)*k
        return ret


startjars :: Int -> IO ()
startjars cnt
    | cnt <= 0 = putStrLn ""
    | otherwise = do
        x_temp <- getLine
        let x_t = words x_temp

        let n = read $ x_t!!0 :: Integer
        let m = read $ x_t!!1 :: Int
    
        total <- getlines m

        let t = total `div` n
        print t

