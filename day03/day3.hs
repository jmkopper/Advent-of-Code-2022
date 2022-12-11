module Main where
import Data.String( fromString )
import Data.Text( splitOn, Text, unpack )
import Data.List( intersect )
import Data.Char( ord )


concat3 :: [a] -> [[a]]
concat3 [] = [[]]
concat3 xs = take 3 xs : concat3 (drop 3 xs)

priority :: Char -> Int
priority c
    | ord c <= 90 = ord c - 38
    | ord c > 90 = ord c - 96
    | otherwise = 0


main :: IO ()
main = do
    raw_data <- readFile "input.txt"
    let lines = init $ map unpack (splitOn (fromString "\n") (fromString raw_data))
    let badges = map priority $ init $ map (head . foldl intersect (['a'..'z'] ++ ['A'..'Z'])) (concat3 lines)
    print $ sum badges
