module MatrixTracing
    ( matrixTracing,
      matrixTracingCases
    ) where
  
import Numeric
import Data.Char
import Data.Array

fst1 (x,_,_) = x
snd1 (_,y,_) = y
trd1 (_,_,z) = z

marray    :: Array Int (Int, Int, Int)
marray =  a  where a = array (0,1999999) ([(0, (1, 1, 1)), (1, (1, 1, 1))] ++ 
                                     [(i,(((1000000007-(1000000007 `div` i) ) * ( fst1 (a!(1000000007 `mod` i)))) `mod` 1000000007,(i * (snd1 (a!(i-1))) ) `mod` 1000000007,( (((1000000007-(1000000007 `div` i) ) * ( fst1 (a!(1000000007 `mod` i)))) `mod` 1000000007) * (trd1 (a!(i-1))   )  ) `mod` 1000000007)) | i <- [2..1999999]])

                       
                                   
matrixTracingCases :: Int -> Array Int (Int,Int,Int) -> IO()
matrixTracingCases t marr
    | t < 1 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let m = (read $ x_t!!0 :: Int) - 1
        let n = (read $ x_t!!1 :: Int) - 1


        
        print ((( (  (snd1 (marr ! (m + n))) * (trd1 (marr ! m)) ) `mod` 1000000007) * (trd1 ( marr ! n) ) ) `mod` 1000000007)

        matrixTracingCases (t-1) marr


matrixTracing ::  IO ()
matrixTracing = do
    n_temp <- getLine
    let t = read n_temp :: Int
    let arr = marray
    matrixTracingCases t arr
    
