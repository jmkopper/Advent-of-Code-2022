module Main where
import Data.String( fromString )
import Data.Char ( ord )
import Data.Text( splitOn, unpack )
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

elevation :: Char -> Int
elevation x = case x of
    'S' -> 1
    'E' -> 26
    _ -> ord x - 96

validNeighbors :: [String] -> (Int, Int) -> [(Int, Int)]
validNeighbors graph (x, y) = filter (\(x', y') -> elevation (graph !! x !! y) >= elevation (graph !! x' !! y') - 1)[(x', y') |
   x' <- [x - 1 .. x + 1],
   0 <= x', x' < length graph,
   y' <- [y - 1 .. y + 1],
   0 <= y', y' < length (head graph),
   x' == x || y' == y, (x', y') /= (x, y)]

pathLength :: Map.Map (Int, Int) (Int, Int) -> (Int, Int) -> (Int, Int) -> Int
pathLength parents start end
    | end == start = 0
    | otherwise = 1 + pathLength parents start (parents Map.! end)

bfs :: [String] -> (Int, Int) -> (Int, Int) -> [(Int, Int)] -> Map.Map (Int, Int) Bool -> Map.Map (Int, Int) (Int, Int) -> Maybe Int
bfs graph start end [] seen parents = Nothing
bfs graph start end (q:queue) seen parents
    | q == end = Just (pathLength parents start end)
    | otherwise = bfs graph start end (queue ++ z) (Map.union seen (Map.fromList [(z', True) | z' <- z])) (Map.union (Map.fromList [(z', q) | z' <- z]) parents)
        where z = filter (\x -> not $ x `Map.member` seen) (validNeighbors graph q)


main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let graph = init $ map unpack $ splitOn (fromString "\n") (fromString rawData)
    let end = (20, 107) -- precomputed
    let aSet = [(x, y) | x <- [0..length graph - 1], y <- [0..length (head graph) - 1], graph !! x !! y == 'a' || graph !! x !! y == 'S']
    let aDists = map (\a -> bfs graph a end [a] (Map.fromList [(a, True)]) (Map.fromList [(a, a)])) aSet
    print $ minimum (catMaybes aDists)
