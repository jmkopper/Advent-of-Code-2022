module Main where
import Data.List.Split ( splitOn )
import Data.HashSet ( HashSet, member, fromList, insert )

neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors (x, y, z) = [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]

surfaceArea :: [(Int, Int, Int)] -> HashSet (Int, Int, Int) -> Int -> Int
surfaceArea [] _ total = total
surfaceArea (x:xs) seen total = surfaceArea xs (insert x seen) z
    where z = total + 6 - 2 * length (filter (`member` seen) (neighbors x))

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let coords = map ((\x -> (read $ head x :: Int, read $ x !! 1 :: Int, read $ x !! 2 :: Int)) . splitOn ",") (filter (/= "") (splitOn "\n" rawData))
    print $ surfaceArea coords (fromList []) 0 --part 1
