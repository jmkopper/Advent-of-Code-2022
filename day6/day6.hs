module Main where
import Data.List( nub )

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let n = 14
    print $ head (filter (\y -> isMarker (take n (drop y rawData)) n) [1..length rawData - n]) + n
