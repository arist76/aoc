import Control.Monad
import Data.Char
import Data.List
import Data.String
import System.IO
import qualified Data.Map as Map

parsedLocations :: String -> [[String]]
parsedLocations inp = map words $ lines inp

left :: [[String]] -> [Int]
left xs = [read (head x) :: Int | x <- xs]

right :: [[String]] -> [Int]
right xs = sort $ [read (head $ tail x) :: Int | x <- xs]

rightSum :: Int -> [Int] -> Int
rightSum y [] = 0
rightSum y (x:xs) = if y == x then x + (rightSum y xs)  else (rightSum y xs)

traverseLeftRight :: [Int] -> [Int] -> Int
traverseLeftRight [] ys = 0
traverseLeftRight (x:xs) ys = (rightSum x ys) + (traverseLeftRight xs ys)

main = do
    handle <- openFile "input.txt" ReadMode
    locations <- hGetContents handle
    putStrLn $ show $ traverseLeftRight  (left $ parsedLocations locations) (right $ parsedLocations locations) 
    hClose handle
