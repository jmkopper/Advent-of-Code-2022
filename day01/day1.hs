module Main where
import Data.List( sortBy )
import Data.Ord( comparing )
import Data.String( fromString )
import Data.Text( splitOn, Text )
import Data.Text.Read
import Data.Either( fromRight )


-- Compute the score of an elf given their string data e.g. "123\n456\n789" -> 123 + 456 + 789
-- elfScore :: Text -> Int
-- elfScore xs = sum (map (fst . fromRight (0, fromString "") . decimal) (splitOn (fromString "\n") xs))

-- Compute the biggest three ints in a list, starting with c3
-- bestThree :: [Int] -> [Int] -> [Int]
-- bestThree xs c3 = foldl (\c3 x -> take 3 $ sortBy (flip compare) (x:c3)) c3 xs


main :: IO ()
main = readFile "./input.txt" >>= print . (\fs -> sum $ foldl (\y x -> take 3 $ sortBy (flip compare) (x:y)) (map (sum . (map (fst . fromRight (0, fromString "") . decimal) . splitOn (fromString "\n"))) (splitOn (fromString "\n\n") (fromString fs))) [0,0,0])
