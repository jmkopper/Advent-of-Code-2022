module Main where
import Data.String( fromString )
import Data.Char ( ord )
import Data.List ( nub, foldl', intercalate )
import Data.List.Split ( splitOn, chunksOf )

data Obj = Air | Rock | Sand deriving Eq

instance Show Obj where
    show Air = "."
    show Rock = "#"
    show Sand = "o"

data Cave = Cave { objects :: [[Obj]], sandSource :: (Int, Int) }

instance Show Cave where
    show cave = unlines (map show (objects cave))


lineToCoord :: String -> [(Int, Int)]
lineToCoord xs = map (\x -> (read (head x), read (last x))) (chunksOf 2 (concatMap (splitOn  ",") (splitOn " -> " xs)))

rockCoord :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
rockCoord (x1, y1) (x2, y2)
    | x1 == x2 = [(x1, z) | z <- [(min y1 y2)..(max y1 y2)]]
    | otherwise = [(z, y1) | z <- [(min x1 x2)..(max x1 x2)]]

rockCoordFromLines :: [String] -> [(Int, Int)]
rockCoordFromLines ls = nub . concat $ concatMap ((\x -> zipWith rockCoord x (tail x)) . lineToCoord) ls

airCaveObjs :: Int -> Int -> [[Obj]]
airCaveObjs width height = replicate height (replicate width Air)

caveFloorObjs :: Int -> [[Obj]]
caveFloorObjs width = [replicate width Air, replicate width Rock]

updateObjsGrid :: [[a]] -> (Int, Int) -> a -> [[a]]
updateObjsGrid grid (x, y) new = take y grid ++ [take x (grid !! y) ++ [new] ++ drop (x + 1) (grid !! y)] ++ drop (y + 1) grid

moveSand :: Cave -> (Int, Int) -> (Int, Int) -> Cave
moveSand cave from to = Cave { objects = updateObjsGrid (updateObjsGrid (objects cave) to Sand) from Air, sandSource = sandSource cave }

caveFromRocks :: [(Int, Int)] -> Cave
caveFromRocks rocks = Cave { objects = foldl' (\g coord -> updateObjsGrid g (fst coord - xoffset, snd coord) Rock) (airCaveObjs width height) rocks ++ [replicate width Air, replicate width Rock], sandSource = (500 - xoffset, 0) }
    where
        xs = [fst a | a <- rocks]
        ys = [snd a | a <- rocks]
        width = 3 * (maximum xs - minimum xs + 5)
        xoffset = minimum xs - (width `div` 3)
        height = maximum ys + 1

simulationStep :: Cave -> (Int, Int) -> (Cave, (Int, Int))
simulationStep cave sc
    | below == Air = (moveSand cave sc (fst sc, snd sc + 1), (fst sc, snd sc + 1))
    | belowLeft == Air = (moveSand cave sc (fst sc - 1, snd sc + 1), (fst sc - 1, snd sc + 1))
    | belowRight == Air = (moveSand cave sc (fst sc + 1, snd sc + 1), (fst sc + 1, snd sc + 1))
    | otherwise = (cave, sc)
    where
        below = (objects cave !! (snd sc + 1)) !! fst sc
        belowLeft = (objects cave !! (snd sc + 1)) !! (fst sc - 1)
        belowRight = (objects cave !! (snd sc + 1)) !! (fst sc + 1)

simulateGrain :: Cave -> (Int, Int) -> Cave
simulateGrain cave sc
    | newsc == sc = cave
    | otherwise = simulateGrain newcave newsc
    where (newcave, newsc) = simulationStep cave sc

main :: IO ()
main = do
    rawData <- readFile "test.txt"
    let lines = splitOn "\n" rawData
    let rocks = rockCoordFromLines lines
    let cave = caveFromRocks rocks
    let (ncave, nsc) = simulationStep cave (fst $ sandSource cave, snd (sandSource cave) + 5)
    let (tcave, tsc) = simulationStep ncave nsc
    let (ucave, usc) = simulationStep tcave tsc
    let (vcave, vsc) = simulationStep ucave usc
    print $ cave
    print $ simulationStep ncave nsc
    print $ simulationStep tcave tsc
    print $ simulationStep ucave usc
    print $ simulationStep vcave vsc
    print $ (objects cave !! 9) !! 20
    print $ (objects ucave !! 9) !! 20
    print $ (objects vcave !! 9) !! 20
    -- print $ simulateGrain cave (sandSource cave)
