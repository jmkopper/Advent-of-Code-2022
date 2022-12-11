module Main where
import Data.String( fromString )
import Data.Text( splitOn, Text, unpack )
import Data.Text.Read( decimal )
import Data.Either( fromRight )


getLines :: String -> [Text]
getLines = init . splitOn (fromString "\n") . fromString

splitLine :: Text -> [Text]
splitLine = splitOn (fromString ",")

strToInt :: Text -> Int
strToInt = fst . (fromRight (0, fromString "") . decimal)

getInterval :: Text -> [Int]
getInterval = map strToInt . splitOn (fromString "-")

overlap :: [Int] -> [Int] -> Bool
overlap a b = (head a <= head b && last a >= head b) || (head b <= head a && last b >= head a)

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let overlaps = length $ filter (\x -> overlap (head x) (last x)) (map ((\x -> [getInterval (head x), getInterval (last x)]) . splitLine) (getLines rawData))
    print overlaps
