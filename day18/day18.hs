module Main where
import Data.List ( foldl' )
import Data.List.Split ( splitOn )
import Data.HashSet ( HashSet, member, fromList, insert, union, toList )

parseLine :: String -> (Int, Int, Int)
parseLine line = (x, y, z)
  where [x, y, z] = map (read :: String -> Int) (splitOn "," line)

neighbors :: (Int, Int, Int) -> [(Int, Int, Int)]
neighbors (x, y, z) = [(x+1, y, z), (x-1, y, z), (x, y+1, z), (x, y-1, z), (x, y, z+1), (x, y, z-1)]

freeNeighbors :: (Int, Int, Int) -> [(Int, Int)] -> HashSet (Int, Int, Int) -> [(Int, Int, Int)]
freeNeighbors coord bounds droplet = [(x', y', z') | (x', y', z') <- neighbors coord,
                                                                    x' >= fst (head bounds) && x'<= snd (head bounds),
                                                                    y' >= fst (bounds !! 1) && y' <= snd (bounds !! 1),
                                                                    z' >= fst (bounds !! 2) && z' <= snd (bounds !! 2),
                                                                    not (coord `member` droplet)]

surfaceArea :: [(Int, Int, Int)] -> HashSet (Int, Int, Int) -> Int -> Int
surfaceArea [] _ total = total
surfaceArea (x:xs) seen total = surfaceArea xs (insert x seen) z
    where z = total + 6 - 2 * length (filter (`member` seen) (neighbors x))

findExterior :: [(Int, Int, Int)] -> HashSet (Int, Int, Int) -> HashSet (Int, Int, Int) -> [(Int, Int)] -> HashSet (Int, Int, Int)
findExterior [] _ v _ = v
findExterior queue droplet visited bounds = findExterior (tail queue ++ z) droplet (visited `union` fromList z) bounds
    where z = filter (\x -> not (x `member` visited)) (freeNeighbors (head queue) bounds droplet)

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let coords = map parseLine (filter (/= "") (splitOn "\n" rawData))
        coordSet = fromList coords
        sArea = surfaceArea coords (fromList []) 0
        bounds = [(-1, maximum [x+1 | (x, _, _) <- coords]),
                  (-1, maximum [y+1 | (_, y, _) <- coords]),
                  (-1, maximum [z+1 | (_, _, z) <- coords])]
        exterior = findExterior [(-1, -1, -1)] coordSet (fromList [(-1, -1, -1)]) bounds
        interior = [(x, y, z) | x <- [0..snd (head bounds)],
                                y <- [0..snd (bounds !! 1)],
                                z <- [0..snd (last bounds)],
                                not $ (x, y, z) `member` exterior,
                                not $ (x, y, z) `member` coordSet]
    putStrLn $ "Part 1:" ++ show sArea
    putStrLn $ "Part 2:" ++ show (sArea - surfaceArea interior (fromList []) 0 )
