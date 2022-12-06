module Main where
import Data.List( nub )

findMarker :: Eq a => [a] -> Int -> Int -> Int
findMarker seq len index = if nub (take len seq) == take len seq
    then index + len
    else findMarker (drop 1 seq) len (index + 1)

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    print $ findMarker rawData 14 0
