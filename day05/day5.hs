module Main where
import Data.String( fromString )
import Data.Text( splitOn, Text, unpack )
import Data.Text.Read( decimal )
import Data.Either( fromRight )
import Data.List (foldl')


getFilePieces :: String -> [Text]
getFilePieces = splitOn (fromString "\n\n") . fromString

getLines :: Text -> [String]
getLines xs = map unpack (init $ splitOn (fromString "\n") xs)

strToInt :: Text -> Int
strToInt = fst . (fromRight (0, fromString "") . decimal)

parseStackLine :: String -> String
parseStackLine "" = ""
parseStackLine xs = (xs !! 1) : parseStackLine (drop 4 xs)

formStacks :: [[a]] -> [[a]]
formStacks ([]:_) = []
formStacks xs = map head xs : formStacks (map tail xs)

parseInstruction :: String -> [Int]
parseInstruction xs = map strToInt [y !! 1, y !! 3, y !! 5]
    where y = splitOn (fromString " ") (fromString xs)

addToStack :: Int -> String -> [String] -> [String]
addToStack i a stacks = take i stacks ++ [(stacks !! i) ++ a] ++ drop (i+1) stacks

dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n xs = dropN (n-1) (init xs)

dropFromStack :: Int -> Int -> [String] -> [String]
dropFromStack i amt stack = take i stack ++ [dropN amt (stack !! i)] ++ drop (i+1) stack

doInstruction :: Int -> Int -> Int -> [String] -> [String]
doInstruction count start end stacks = dropFromStack (start-1) count $ addToStack (end-1) (reverse (take count (reverse (stacks !! (start-1))))) stacks

instruct :: [Int] -> [String] -> [String]
instruct n = doInstruction (n !! 0) (n !! 1) (n !! 2)

main :: IO ()
main = do
    rawData <- readFile "input.txt"
    let filePieces = getFilePieces rawData
    let instructions = map parseInstruction (getLines (filePieces !! 1))
    let stacks = map (reverse . filter (/=' ')) (formStacks $ map parseStackLine (getLines (head filePieces)))
    let finalStacks = foldl' (flip instruct) stacks instructions
    print $ map last finalStacks
