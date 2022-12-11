module Main where
import Data.String( fromString )
import Data.Text( splitOn, Text, unpack )
import Data.Text.Read ()

vs :: [String] -> Int
vs ["A", "X"] = 3
vs ["A", "Y"] = 4
vs ["A", "Z"] = 8
vs ["B", "X"] = 1
vs ["B", "Y"] = 5
vs ["B", "Z"] = 9
vs ["C", "X"] = 2
vs ["C", "Y"] = 6
vs ["C", "Z"] = 7
vs _ = 0

main :: IO ()
main = readFile "input.txt" >>= print . (sum . map (vs . (map unpack . splitOn (fromString " "))) . splitOn (fromString "\n") . fromString)
