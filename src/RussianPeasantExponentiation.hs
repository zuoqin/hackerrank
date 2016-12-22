module RussianPeasantExponentiation
    ( russianPeasantExponentiation,
      russianPeasantExponentiationCases
    ) where
  
import Numeric
import Data.Char
import Data.Set as Set

fibonaci :: Num b => [b]
fibonaci = Prelude.map fst (iterate f (0,1)) where f (x,y) = (y,x+y)

russianPeasantExponentiationCases :: Int -> IO()
russianPeasantExponentiationCases n
    | n <= 0 = putStrLn ""
    | otherwise = do          
        x_temp <- getLine
        let x_t = words x_temp

        let a = read $ x_t!!0 :: Integer
        let b = read $ x_t!!1 :: Integer
        let k = read $ x_t!!1 :: Integer
        let m = read $ x_t!!1 :: Integer

        let r = (sqrt  ((fromInteger a) ^^ 2) + ((fromInteger b) ^^ 2)) ^^ k
        let alpha = snd( properFraction( ((atan 1) * 10) / ( 2 * pi ) ) )

        --let x = fst( properFraction (r * cos( alpha ))  m )
        --let y = fst( properFraction (r * sin (alpha ))  m )

        --putStrLn x 
        russianPeasantExponentiationCases (n-1)
        

russianPeasantExponentiation ::  IO ()
russianPeasantExponentiation = do
    n_temp <- getLine
    let n = read n_temp :: Int
    russianPeasantExponentiationCases n
    
