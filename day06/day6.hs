module Main where
import Data.List( nub )

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let n = 14
    print $ head (filter (\y -> nub (take n (drop y rawData)) == take n (drop y rawData)) [1..length rawData - n]) + n
