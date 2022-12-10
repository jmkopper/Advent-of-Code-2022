module Main where
import Data.String( fromString )
import Data.Text( splitOn, Text, unpack )
import Data.List( foldl' )
import Data.List.Split( chunksOf )


parseLine :: Text -> Maybe Int
parseLine xs
    | unpack (head y) == "addx" = Just ((read. unpack) (last y))
    | otherwise = Nothing
    where y = splitOn (fromString " ") xs


doCommand :: Maybe Int -> [(Int, Int)] -> [(Int, Int)]
doCommand Nothing xs = xs ++ [(snd $ last xs, snd $ last xs)]
doCommand (Just n) xs = xs ++ [(snd $ last xs, snd $ last xs)] ++ [(fst $ last xs, snd (last xs) + n)]


drawRow :: [Int] -> Int -> String
drawRow [] _ = ""
drawRow (x:xs) n
    | (x-1) <= n && (x+1) >= n = "#" ++ drawRow xs (n+1)
    | otherwise = "." ++ drawRow xs (n+1)


main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let lines = splitOn (fromString "\n") (fromString rawData)
    let rows = map (`drawRow` 0) $ chunksOf 40 $ take 240 (map snd (foldl' (flip doCommand) [(1,1)] $ map parseLine lines))
    mapM_ print rows
