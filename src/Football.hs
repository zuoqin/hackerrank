-- Нужно составить календарь соревнования из N команд. Каждый день играют не более трёх команд.
--Команда не должна играть два дня подряд. Команды должны состязаться между собой один раз.

module Football
    ( football,
      tupleslisttointlist,
      searcharr,
      get1st,
      get2nd,
      tripletupletodouble,
      addpairtothisday
    ) where

import Data.Char
import Data.List

tupleslisttointlist :: [(Int,Int)] -> [Int]
tupleslisttointlist pairs = foldl (\xs (x,y) -> x : y : xs ) [] pairs

searcharr :: Int -> [Int] -> Int
searcharr x prevdays =
   length (filter (\y -> y == x) prevdays)

get1st (a,_,_) = a
get2nd (_,a,_) = a

isthisday :: [(Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int,Int)]
isthisday pairs thisday = do
  map (\(x,y,z) -> if length (filter (\(m,n) -> (m==x) &&(y==n)) thisday) == 0 then (x,y,z) else (x,y,1) ) pairs

tripletupletodouble :: [(Int,Int,Int)] -> [(Int,Int)]
tripletupletodouble pairs = map (\(x,y,_) -> (x,y)) pairs

addpairtothisday :: (Int,Int,Int) -> [(Int,Int)] -> [(Int,Int)]
addpairtothisday pair thisday = (get1st pair, get2nd pair) : thisday

buildthisday :: [(Int,Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
buildthisday pairs thisday = do
  let z = (filter (\(x,y,z) -> (z==0) && (searcharr x (tupleslisttointlist thisday) == 0) && (searcharr y (tupleslisttointlist thisday) == 0))  pairs)

  let selected = head z
  let newpairs = if (length z) == 0 then pairs else map (\(x,y,z) -> if (x== get1st selected) && (y == get2nd selected) then (x,y,1) else (x,y,z)) pairs
  if (length thisday == 3) || (length z == 0) then thisday else buildthisday newpairs (addpairtothisday selected thisday)

printday :: [(Int,Int,Int)] -> [Int] -> [(Int,Int)]
printday pairs prevdays = do
  let allteams = (nub . tupleslisttointlist . tripletupletodouble) pairs
  let resteams = filter (\x -> (searcharr x prevdays == 0) && (searcharr x prevdays == 0) ) allteams

  let newarr = filter (\(x,y,z) -> (searcharr x resteams > 0) && (searcharr y resteams > 0) && z == 0) pairs

  buildthisday newarr []


printschedule :: [(Int,Int,Int)] -> [(Int,Int)] -> IO()
printschedule pairs prevday = do
  let thisday = printday pairs (tupleslisttointlist prevday)


  print thisday
  putStrLn ""
  let newpairs = isthisday pairs thisday
  let respairs = filter (\(_,_,z) -> z == 0) newpairs
  if length respairs > 0 then printschedule respairs thisday else putStrLn ""
   
  --printschedule respairs thisday
  --print respairs
  

outerProduct xs ys =
   do
       x <- xs          -- for each x drawn from xs:
       y <- ys          -- for each y drawn from ys:
       return (x,y,0)   -- produce the (x,y) pair


football :: IO ()
football = do
    k_temp <- getLine
    let k = read k_temp :: Int

    printschedule (filter (\(x,y,_) -> x < y) ( outerProduct [1 .. k] [2 .. k] )) []
