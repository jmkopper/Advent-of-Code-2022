module Main where
import Data.List.Split ( splitOn )
import qualified Data.HashMap.Strict as Map
import Data.Maybe ( catMaybes, mapMaybe )

snafuMap = Map.fromList [('1', 1), ('2', 2), ('0', 0), ('-', -1), ('=', -2)]
snafuInv = [("0", 0), ("1", 0), ("2", 0), ("=", 1), ("-", 1), ("0", 1)]

toDec :: String -> Int
toDec xs = sum $ zipWith (\ x y -> x * 5 ^ y) (mapMaybe (`Map.lookup` snafuMap) (reverse xs)) [0..]

toSnafu :: Int -> Int -> String
toSnafu 0 0 = ""
toSnafu 0 k = fst (snafuInv !! k)
toSnafu n k = toSnafu (n `div` 5) (snd z) ++ fst z
    where z = snafuInv !! (n `mod` 5 + k)

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    print $ toSnafu (sum (map toDec (splitOn "\n" rawData))) 0
